package com.srednal.snug.actor

import akka.actor.{ActorRef, ActorNotFound, Actor, ActorSystem}
import akka.actor.ActorDSL._
import akka.testkit.TestActorRef
import org.scalatest._
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import spray.util.pimpFuture

class PubSub2Test extends WordSpec with Matchers {
  import PubSub2._

  implicit val actorSystem = ActorSystem("test")

  "The PubSub2 object" should {

    val emptyActor = TestActorRef(new Actor {def receive = { case _ => }})

    "construct a simple sub-channel message" in {
      channelParent("sub") shouldBe None
    }
    "construct a sub-sub-channel message" in {
      channelParent("outer/inner/foo") shouldBe  Some("outer/inner")
    }

    "reply Subscribed" in {
      (PubSub2 ? Subscribe(emptyActor, None)).await shouldBe Subscribed
      (PubSub2 ? Subscribe(emptyActor, None)).await shouldBe Subscribed
    }

    "reply Subscribed (via !)" in {
      implicit val in = inbox()
      PubSub2 ! Subscribe(emptyActor, None)
      in.receive() shouldBe Subscribed
    }

    "reply Unsubscribed" in {
      (PubSub2 ? Unsubscribe(emptyActor, None)).await shouldBe Unsubscribed
    }

    "reply Subscribed for sub-channels" in {
      (PubSub2 ? Subscribe(emptyActor, Some("outer"))).await shouldBe Subscribed
    }
    "reply Subscribed for sub-sub-channels" in {
      (PubSub2 ? Subscribe(emptyActor, Some("outer/inner"))).await shouldBe Subscribed
    }

    "reply Unsubscribed for sub-channels" in {
      (PubSub2 ? Unsubscribe(emptyActor, Some("outer"))).await shouldBe Unsubscribed
    }
    "reply Unsubscribed for sub-sub-channels" in {
      (PubSub2 ? Unsubscribe(emptyActor, Some("outer/inner"))).await shouldBe Unsubscribed
    }

    "reply subscribed from subscribe func" in {
      subscribe(emptyActor, Some("outer/inner")).await shouldBe Subscribed
    }
    "reply unsubscribed from unsubscribe func" in {
      unsubscribe(emptyActor, Some("outer/inner")).await shouldBe Unsubscribed
    }
  }


  "The PubSub2 Actor" should {


    "deliver messages to subscribed actors" in {
      val listener = inbox()
      subscribe(listener.receiver, None).await

      PubSub2 ! Message("hello there", None)
      listener.receive() shouldBe "hello there"
    }

    "stop delivery to unsubscribed actors" in {
      val listener = inbox()
      subscribe(listener.receiver, None).await

      PubSub2 ! Message("hello there you", None)
      listener.receive() shouldBe "hello there you"

      PubSub2 ! Message("hello there more", None)
      listener.receive() shouldBe "hello there more"

      unsubscribe(listener.receiver, None).await

      PubSub2 ! Message("hello there again", None)
      a[TimeoutException] should be thrownBy listener.receive(250.millis)
    }


    "deliver root messages to subscribed actors in sub-channels" in {
      val listener = inbox()
      subscribe(listener.receiver, Some("test/sub")).await

      PubSub2 ! Message("hello subs", Some("test"))
      listener.receive() shouldBe "hello subs"
    }

    "subscribing to a sub-channel implies subscription to parents also" in {
      val listener0 = inbox()
      val listener1 = inbox()
      val listener2 = inbox()
      val listener3 = inbox()

      subscribe(listener0.receiver, None).await
      subscribe(listener1.receiver, Some("test1")).await
      subscribe(listener2.receiver, Some("test1/sub1")).await
      subscribe(listener3.receiver, Some("test1/sub1/sub2")).await

      PubSub2 ! Message("hello top", None)
      listener0.receive() shouldBe "hello top"
      listener1.receive() shouldBe "hello top"
      listener2.receive() shouldBe "hello top"
      listener3.receive() shouldBe "hello top"
    }

    "publish to a sub-channel does not deliver to parents" in {
      val listener0 = inbox()
      val listener1 = inbox()
      val listener2 = inbox()
      val listener3 = inbox()
      subscribe(listener0.receiver, None).await
      subscribe(listener1.receiver, Some("test2")).await
      subscribe(listener2.receiver, Some("test2/sub1")).await
      subscribe(listener3.receiver, Some("test2/sub1/sub2")).await

      PubSub2 ! Message("hello down", Some("test2/sub1"))
      listener2.receive() shouldBe "hello down"
      listener3.receive() shouldBe "hello down"
      a[TimeoutException] should be thrownBy listener0.receive(250.millis)
      a[TimeoutException] should be thrownBy listener1.receive(250.millis)
    }
  }
}
