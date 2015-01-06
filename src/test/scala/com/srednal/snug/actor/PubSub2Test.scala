package com.srednal.snug.actor

import akka.actor.{Actor, ActorSystem}
import akka.actor.ActorDSL._
import akka.testkit.TestActorRef
import com.srednal.snug.Path
import com.srednal.snug.Path._
import org.scalatest._
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import spray.util.pimpFuture

// IMO being explicit (fewer constants) is good in tests, import scoping is intentional
// scalastyle:off magic.number multiple.string.literals import.grouping

class PubSub2Test extends WordSpec with Matchers {
  import PubSub2._

  implicit val actorSystem = ActorSystem("test")

  "The PubSub2 object" should {

    val emptyActor = TestActorRef(new Actor {def receive = { case _ => }})

    "reply Subscribed" in {
      (PubSub2 ? Subscribe(emptyActor, ^)).await shouldBe Subscribed
      (PubSub2 ? Subscribe(emptyActor, ^)).await shouldBe Subscribed
    }

    "reply Subscribed (via !)" in {
      implicit val in = inbox()
      PubSub2 ! Subscribe(emptyActor, ^)
      in.receive() shouldBe Subscribed
    }

    "reply Unsubscribed" in {
      (PubSub2 ? Unsubscribe(emptyActor, ^)).await shouldBe Unsubscribed
    }

    "reply Subscribed for sub-channels" in {
      (PubSub2 ? Subscribe(emptyActor, "outer".asPath)).await shouldBe Subscribed
    }
    "reply Subscribed for sub-sub-channels" in {
      (PubSub2 ? Subscribe(emptyActor, "outer" / "inner")).await shouldBe Subscribed
    }

    "reply Unsubscribed for sub-channels" in {
      (PubSub2 ? Unsubscribe(emptyActor, ^ / "outer")).await shouldBe Unsubscribed
    }
    "reply Unsubscribed for sub-sub-channels" in {
      (PubSub2 ? Unsubscribe(emptyActor, "outer" / "inner")).await shouldBe Unsubscribed
    }

    "reply subscribed from subscribe func" in {
      subscribe(emptyActor, "outer/inner").await shouldBe Subscribed
    }
    "reply unsubscribed from unsubscribe func" in {
      unsubscribe(emptyActor, "outer/inner").await shouldBe Unsubscribed
    }
  }


  "The PubSub2 Actor" should {


    "deliver messages to subscribed actors" in {
      val listener = inbox()
      subscribe(listener.receiver, ^).await

      PubSub2 ! Message("hello there", ^)
      listener.receive() shouldBe "hello there"
    }

    "stop delivery to unsubscribed actors" in {
      val listener = inbox()
      subscribe(listener.receiver, ^).await

      publish("hello there you", ^)
      listener.receive() shouldBe "hello there you"

      publish("hello there more", ^)
      listener.receive() shouldBe "hello there more"

      unsubscribe(listener.receiver, ^).await

      publish("hello there again", ^)
      a[TimeoutException] should be thrownBy listener.receive(250.millis)
    }


    "deliver root messages to subscribed actors in sub-channels" in {
      val listener = inbox()
      subscribe(listener.receiver, "test/sub").await

      publish("hello subs", "test")
      listener.receive() shouldBe "hello subs"
    }

    "subscribing to a sub-channel implies subscription to parents also" in {
      val listener0 = inbox()
      val listener1 = inbox()
      val listener2 = inbox()
      val listener3 = inbox()

      subscribe(listener0.receiver, ^).await
      subscribe(listener1.receiver, "test1").await
      subscribe(listener2.receiver, "test1/sub1").await
      subscribe(listener3.receiver, "test1" / "sub1" / "sub2").await

      publish("hello top", ^)
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
      subscribe(listener0.receiver, ^).await
      subscribe(listener1.receiver, "test2").await
      subscribe(listener2.receiver, "test2/sub1").await
      subscribe(listener3.receiver, "test2/sub1/sub2").await

      publish("hello down", "test2/sub1")
      listener2.receive() shouldBe "hello down"
      listener3.receive() shouldBe "hello down"
      a[TimeoutException] should be thrownBy listener0.receive(250.millis)
      a[TimeoutException] should be thrownBy listener1.receive(250.millis)
    }
  }
}
