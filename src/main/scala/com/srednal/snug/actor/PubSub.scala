package com.srednal.snug.actor

import akka.actor._
import akka.pattern.ask
import akka.routing.{ActorRefRoutee, BroadcastRoutingLogic, Router}
import akka.util.Timeout
import com.srednal.snug.Path
import com.srednal.snug.Path._
import com.srednal.snug.log.Logger
import com.srednal.snug.config._
import scala.concurrent.Future

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
  implicit val timeout = config.as[Timeout]("pubsub.select-timeout")

  private val rootActor = actorSystem.actorOf(Props(new PubSub(true)), "pubsub")

  /** A subscribe message - subscribe receiver to channel. */
  case class Subscribe(receiver: ActorRef, channel: Path)

  /** Subscribe responds to the sender with Subscribed */
  case object Subscribed

  /** An unsubscribe message - unsub receiver from channel. */
  case class Unsubscribe(receiver: ActorRef, channel: Path)

  /** Unsubscribe responds to the sender with Unsubscribed */
  case object Unsubscribed

  /** A wrapper around the message to also hold its destination.  The actual message delivered to the subscribers is msg. */
  case class Message(msg: Any, channel: Path)

  /** Send a message (via tell) */
  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit = rootActor ! message

  /** Send a message (via ask) */
  def ?(message: Any)(implicit timeout: Timeout): Future[Any] = rootActor ? message

  /** Publish a message (simple async) */
  def publish(msg: Any, channelPath: String): Unit = publish(msg, channelPath.asPath)
  def publish(msg: Any, channel: Path): Unit = publish(Message(msg, channel))
  def publish(message: Message): Unit = this ! message


  /** Subscribe */
  def subscribe(receiver: ActorRef, channelPath: String): Future[Subscribed.type] = subscribe(receiver, channelPath.asPath)
  def subscribe(receiver: ActorRef, channel: Path): Future[Subscribed.type] =
    (this ? Subscribe(receiver, channel)).mapTo[Subscribed.type]

  /** Unsubscribe */
  def unsubscribe(receiver: ActorRef, channelPath: String)(implicit timeout: Timeout): Future[Unsubscribed.type] = unsubscribe(receiver, channelPath.asPath)
  def unsubscribe(receiver: ActorRef, channel: Path)(implicit timeout: Timeout): Future[Unsubscribed.type] =
    (this ? Unsubscribe(receiver, channel)).mapTo[Unsubscribed.type]


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

class PubSub(val isRoot: Boolean = false) extends Actor {
  import PubSub._

  val isChild = !isRoot

  val router = new BroadcastRouter

  def channelActor(ch: String, create: Boolean) =
    context.actorSelection(ch).resolveOne() recover {
      case _: ActorNotFound if create =>
        val a = context.actorOf(Props(new PubSub), ch)
        context watch a
        // all messages to us will also be routed to our children
        router += a
        a
    }

  override def receive = {

    // subscribe to ME!
    case Subscribe(receiver, ^) =>
      router += receiver
      sender() ! Subscribed

    // subscribe to one of my sub-channels
    case Subscribe(receiver, Path(outer, inner@_*)) =>
      val sndr = sender()
      channelActor(outer, true) map {_ tell(Subscribe(receiver, Path(inner)), sndr)}


    // they don't like me anymore
    case Unsubscribe(receiver, ^) =>
      router -= receiver
      // shutdown if I have no children left to route to
      // this has race conditions if we have sub-channel subscribe messages in our queue
      // if (isChild && router.isEmpty) context stop self
      sender() ! Unsubscribed

    // un-subscribe from a sub-channel
    case Unsubscribe(receiver, Path(outer, inner@_*)) =>
      val sndr = sender()
      channelActor(outer, false) map {_ tell(Unsubscribe(receiver, Path(inner)), sndr)}


    case Terminated(child) =>
      // remove terminated child actor
      router -= child
    //      if (isChild && router.isEmpty) context stop self


    // a message for me - route it
    case Message(m, ^) => self forward m

    // route a message to subchannel(s)
    case Message(m, Path(outer, inner@_*)) =>
      val sndr = sender()
      channelActor(outer, false) map {_ tell(Message(m, Path(inner)), sndr)}


    // base message routing
    // this will also send this raw message to my subchannel actors, as they are also registered as routes
    case m if router.nonEmpty => router.route(m, sender())

    // avoid some of the dead-letter logging
    case deadLetter => log.debug(s"no route for: $deadLetter")
  }
}
