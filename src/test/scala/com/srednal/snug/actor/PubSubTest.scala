package com.srednal.snug.actor

import akka.actor.{Actor, ActorSystem}
import akka.actor.ActorDSL._
import akka.testkit.TestActorRef
import org.scalatest._
import java.util.concurrent.TimeoutException
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import akka.pattern.ask

class PubSubTest extends WordSpec with Matchers {
  import PubSub._

  implicit val actorSystem = ActorSystem("test")

  "The PubSub object" should {

    val emptyActor = TestActorRef(new Actor {def receive = { case _ => }})

    "construct a simple sub-channel message" in {
      splitChannel("sub") shouldBe("sub", None)
    }
    "construct a sub-sub-channel message" in {
      splitChannel("outer/inner/foo") shouldBe("outer", Some("inner/foo"))
    }

    "reply Subscribed" in {
      Await.result(rootActor ? Subscribe(emptyActor, None), 1.second) shouldBe Subscribed
      Await.result(rootActor ? Subscribe(emptyActor, None), 1.second) shouldBe Subscribed
    }
    "reply Unsubscribed" in {
      Await.result(rootActor ? Unsubscribe(emptyActor, None), 1.second) shouldBe Unsubscribed
    }

    "reply Subscribed for sub-channels" in {
      Await.result(rootActor ? Subscribe(emptyActor, Some("outer")), 1.second) shouldBe Subscribed
    }
    "reply Subscribed for sub-sub-channels" in {
      Await.result(rootActor ? Subscribe(emptyActor, Some("outer/inner")), 1.second) shouldBe Subscribed
    }

    "reply Unsubscribed for sub-channels" in {
      Await.result(rootActor ? Unsubscribe(emptyActor, Some("outer")), 1.second) shouldBe Unsubscribed
    }
    "reply Unsubscribed for sub-sub-channels" in {
      Await.result(rootActor ? Unsubscribe(emptyActor, Some("outer/inner")), 1.second) shouldBe Unsubscribed
    }

    "reply subscribed from subscribe func" in {
      implicit val rx = inbox()
      subscribe(emptyActor, Some("outer/inner"))
      rx.receive(1.second) shouldBe Subscribed
    }
    "reply unsubscribed from unsubscribe func" in {
      implicit val rx = inbox()
      unsubscribe(emptyActor, Some("outer/inner"))
      rx.receive(1.second) shouldBe Unsubscribed
    }
  }


  "The PubSub Actor" should {

    "create as branch nodes by default" in {
      TestActorRef(new PubSub).underlyingActor.isRoot shouldBe false
    }

    "create a sub-channel actor" in {
      val rootRef = TestActorRef(new PubSub(true), "pubsub")
      val root = rootRef.underlyingActor
      Await.ready(root.channelActor("foo", false), 1.second).value.get shouldBe a[Failure[_]]
      Await.ready(root.channelActor("foo", true), 1.second).value.get shouldBe a[Success[_]]
      Await.ready(root.channelActor("foo", false), 1.second).value.get shouldBe a[Success[_]]
      Await.ready(root.channelActor("foo", false), 1.second).value.get.get.path shouldBe (rootRef.path / "foo")
    }

    "deliver messages to subscribed actors" in {
      val listener = inbox()
      Await.ready(rootActor ? Subscribe(listener.receiver, None), 1.second)
      rootActor ! Message("hello there", None)
      listener.receive(1.second) shouldBe "hello there"
    }

    "stop delivery to unsubscribed actors" in {
      val listener = inbox()
      Await.ready(rootActor ? Subscribe(listener.receiver, None), 1.second)

      rootActor ! Message("hello there you", None)
      listener.receive(1.second) shouldBe "hello there you"

      rootActor ! Message("hello there more", None)
      listener.receive(1.second) shouldBe "hello there more"

      Await.ready(rootActor ? Unsubscribe(listener.receiver, None), 1.second)

      rootActor ! Message("hello there again", None)
      a[TimeoutException] should be thrownBy listener.receive(250.millis)
    }


    "deliver root messages to subscribed actors in sub-channels" in {
      val listener = inbox()
      Await.ready(rootActor ? Subscribe(listener.receiver, Some("test/sub")), 1.second)
      rootActor ? Message("hello subs", Some("test"))
      listener.receive(1.second) shouldBe "hello subs"
    }

    "subscribing to a sub-channel implies subscription to parents also" in {
      val listener0 = inbox()
      val listener1 = inbox()
      val listener2 = inbox()
      val listener3 = inbox()
      Await.ready(rootActor ? Subscribe(listener0.receiver, None), 1.second)
      Await.ready(rootActor ? Subscribe(listener1.receiver, Some("test1")), 1.second)
      Await.ready(rootActor ? Subscribe(listener2.receiver, Some("test1/sub1")), 1.second)
      Await.ready(rootActor ? Subscribe(listener3.receiver, Some("test1/sub1/sub2")), 1.second)

      rootActor ? Message("hello top", None)
      listener0.receive(1.second) shouldBe "hello top"
      listener1.receive(1.second) shouldBe "hello top"
      listener2.receive(1.second) shouldBe "hello top"
      listener3.receive(1.second) shouldBe "hello top"
    }

    "publish to a sub-channel does not deliver to parents" in {
      val listener0 = inbox()
      val listener1 = inbox()
      val listener2 = inbox()
      val listener3 = inbox()
      Await.ready(rootActor ? Subscribe(listener0.receiver, None), 1.second)
      Await.ready(rootActor ? Subscribe(listener1.receiver, Some("test2")), 1.second)
      Await.ready(rootActor ? Subscribe(listener2.receiver, Some("test2/sub1")), 1.second)
      Await.ready(rootActor ? Subscribe(listener3.receiver, Some("test2/sub1/sub2")), 1.second)

      rootActor ? Message("hello down", Some("test2/sub1"))
      listener2.receive(1.second) shouldBe "hello down"
      listener3.receive(1.second) shouldBe "hello down"
      a[TimeoutException] should be thrownBy listener0.receive(250.millis)
      a[TimeoutException] should be thrownBy listener1.receive(250.millis)
    }
  }
}
