package com.srednal.snug.actor

import akka.actor._
import akka.pattern.ask
import akka.routing.{ActorRefRoutee, BroadcastRoutingLogic, Router}
import akka.util.Timeout
import com.srednal.snug.log.Logger
import com.srednal.snug.config._
import scala.collection.mutable
import scala.concurrent.Future

/**
 * A Pub-Sub hub to play with actors.
 *
 * This is a different way (vs. PubSub) using only one actor and several routers (rather than one router per each of several actors).
 *
 * Subscribe/Unsubscribe to channel.
 * Publish to channel.
 *
 * Channels are a hierarchy.  The top channel (None) can have children "foo", "bar", etc.
 * Those can have children "foo/bar" etc.
 *
 * Messages published to the top will be distributed to all subscribers.
 * Messages published to "foo" will be distributed to all subscribers of "foo" and its sub-channels.
 *
 * Subscribers are actors.
 *
 * A message can be anything.
 */
object PubSub2 {
  val log = Logger(this)

  val actorSystem = ActorSystem("srednal")
  implicit val executionEnv = actorSystem.dispatcher
  implicit val timeout = config.as[Timeout]("pubsub.select-timeout")

  private val actor = actorSystem.actorOf(Props(new PubSub2), "pubsub2")

  /** A subscribe message - subscribe receiver to channel. */
  case class Subscribe(receiver: ActorRef, channel: Option[String])

  /** Subscribe responds to the sender with Subscribed */
  case object Subscribed

  /** An unsubscribe message - unsub receiver from channel. */
  case class Unsubscribe(receiver: ActorRef, channel: Option[String])

  /** Unsubscribe responds to the sender with Unsubscribed */
  case object Unsubscribed

  /** A wrapper around the message to also hold its destination.  The actual message delivered to the subscribers is msg. */
  case class Message(msg: Any, channel: Option[String])

  /** Send a message (via tell) */
  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit = actor ! message

  /** Send a message (via ask) */
  def ?(message: Any)(implicit timeout: Timeout): Future[Any] = actor ? message

  /** Publish a message (simple async) */
  def publish(message: Message): Unit = this ! message

  /** Subscribe */
  def subscribe(receiver: ActorRef, channel: Option[String]): Future[Subscribed.type] =
    (this ? Subscribe(receiver, channel)).mapTo[Subscribed.type]

  /** Unsubscribe */
  def unsubscribe(receiver: ActorRef, channel: Option[String])(implicit timeout: Timeout): Future[Unsubscribed.type] =
    (this ? Unsubscribe(receiver, channel)).mapTo[Unsubscribed.type]

  /** Split "foo/bar..." into "foo" and (optionally) "bar..." */
  private[actor] def channelParent(channel: String): Option[String] = channel.reverse.split("/", 2) match {
    case Array(_, c) => Some(c.reverse)
    case Array(_) => None
  }

  // encapsulate the Router (it needs to be mutable to add/remove routees)
  class BroadcastRouter {
    private var router = Router(BroadcastRoutingLogic())
    def +=(routee: ActorRef): Unit = if (!contains(routee)) router = router.addRoutee(routee)
    def -=(routee: ActorRef): Unit = router = router.removeRoutee(routee)
    def contains(routee: ActorRef): Boolean = router.routees contains ActorRefRoutee(routee)
    def isEmpty: Boolean = router.routees.isEmpty
    def nonEmpty: Boolean = !isEmpty
    def route(message: Any, sender: ActorRef): Unit = router.route(message, sender)
  }

}

class PubSub2 extends Actor {
  import PubSub2._

  val routers = mutable.Map[String,BroadcastRouter]()

  override def receive = {

    // subscribe to one of my sub-channels
    case Subscribe(receiver, channel) =>
      def sub(ch: Option[String]): Unit = {
        routers.getOrElseUpdate(ch.orNull, new BroadcastRouter) += receiver
        // subscribe to parent channels
        ch map channelParent foreach sub
      }
      sub(channel)
      sender() ! Subscribed

    // un-subscribe from a sub-channel
    case Unsubscribe(receiver, channel) =>
      def unsub(ch: Option[String]): Unit = {
        routers get ch.orNull foreach { r =>
          r -= receiver
          if (r.isEmpty) routers -= ch.orNull
        }
        // un-subscribe from parent channels
        ch map channelParent foreach unsub
      }
      unsub(channel)
      sender() ! Unsubscribed

    // route a message
    case Message(m, channel) =>
      routers get channel.orNull map (_.route(m, sender()))

    // avoid some of the dead-letter logging
    case deadLetter => log.debug(s"no route for: $deadLetter")
  }
}
