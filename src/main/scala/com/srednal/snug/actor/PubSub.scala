package com.srednal.snug.actor

import akka.actor._
import akka.pattern.ask
import akka.routing.{ActorRefRoutee, BroadcastRoutingLogic, Router}
import akka.util.Timeout
import com.srednal.snug.log.Logger
import com.srednal.snug.config._

/**
 * A Pub-Sub hub to play with actors.
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
object PubSub {
  val log = Logger(this)

  val actorSystem = ActorSystem("srednal")
  implicit val executionEnv = actorSystem.dispatcher
  implicit val timeout = new Timeout(cdur"pubsub.select-timeout")

  val rootActor = actorSystem.actorOf(Props(new PubSub(true)), "pubsub")

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

  /** Publish a message */
  def publish(message: Message): Unit = rootActor ! message

  /** Subscribe */
  def subscribe(receiver: ActorRef, channel: Option[String])(implicit sender: ActorRef = Actor.noSender): Unit = rootActor ! Subscribe(receiver, channel)

  /** Unsubscribe */
  def unsubscribe(receiver: ActorRef, channel: Option[String])(implicit sender: ActorRef = Actor.noSender): Unit = rootActor ! Unsubscribe(receiver, channel)

  /** Split "foo/bar..." into "foo" and (optionally) "bar..." */
  private[actor] def splitChannel(channel: String): (String, Option[String]) = channel split("/", 2) match {
    case Array(c1, c2) => (c1, Some(c2))
    case Array(c) => (c, None)
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

  // a maintainence sweep
  private object Maint

}

class PubSub(val isRoot: Boolean = false) extends Actor {
  import PubSub._

  val router = new BroadcastRouter

  def channelActor(ch: String, create: Boolean) =
    context.actorSelection(ch).resolveOne() recover {
      case _: ActorNotFound if create =>
        val a = context.actorOf(Props(new PubSub), ch)
        // all messages to us will also be routed to our children
        router += a
        a
    }

  override def receive = {

    // subscribe to one of my sub-channels
    case Subscribe(receiver, Some(channel)) =>
      val sndr = sender()
      val (outer, inner) = splitChannel(channel)
      channelActor(outer, true) map {_ tell(Subscribe(receiver, inner), sndr)}

    // subscribe to ME!
    case Subscribe(receiver, _) =>
      router += receiver
      sender() ! Subscribed

    // unsubscribe from a sub-channel
    case Unsubscribe(receiver, Some(channel)) =>
      val sndr = sender()
      val (outer, inner) = splitChannel(channel)
      channelActor(outer, false) map {_ tell(Unsubscribe(receiver, inner), sndr)}

    // they dont like me anymore
    case Unsubscribe(receiver, _) =>
      router -= receiver
      sender() ! Unsubscribed
      self ! Maint

    // self-check
    case Maint if !isRoot && router.isEmpty =>
      // I have no routees (no subscribers, no children) so I am no longer of use :(
      // remove myself from my parent's router, then stop
      (context.parent ? Unsubscribe(self, None)) onComplete { _ => context stop self}

    // route a message to subchannel(s)
    case Message(m, Some(channel)) =>
      val sndr = sender()
      val (outer, inner) = splitChannel(channel)
      channelActor(outer, false) map {_ tell(Message(m, inner), sndr)}

    // a message for me - route it
    case Message(m, _) => self forward m

    // base message routing
    // this will also send this raw message to my subchannel actors, as they are also registered as routes
    case m if router.nonEmpty => router.route(m, sender())

    // avoid some of the dead-letter logging
    case deadLetter => log.debug(s"no route for: $deadLetter")
  }
}
