package com.srednal.snug

import org.scalatest._

class ServicesLoaderTest extends WordSpec with Matchers {

  import testservices._

  "The ServicesLoader" should {
    "find non-erroring services" in {
      ServicesLoader[Test].map(_.getClass) shouldBe
        classOf[Test3] :: classOf[Test1] :: classOf[Test2] :: Nil
    }

    "not instantiate anything till the stream is accessed" in {
      Test.clear()
      val s = ServicesLoader[Test]
      Test.counts shouldBe Array(0, 0, 1) // does initially instantiate head

      s.head shouldBe a[Test3]
      Test.counts shouldBe Array(0, 0, 1)

      s.head shouldBe a[Test3] // still same instance
      Test.counts shouldBe Array(0, 0, 1)

      s.tail.head shouldBe a[Test1]
      Test.counts shouldBe Array(1, 0, 1)
    }

    "not instantiate things more than once from the same loader" in {
      Test.clear()
      val s = ServicesLoader[Test]
      s map (_.getClass) shouldBe classOf[Test3] :: classOf[Test1] :: classOf[Test2] :: Nil
      s map (_.getClass) shouldBe classOf[Test3] :: classOf[Test1] :: classOf[Test2] :: Nil
      Test.counts shouldBe Array(1, 1, 1)

      // but from a new loader....
      ServicesLoader[Test].toStream.force
      Test.counts shouldBe Array(2, 2, 2)
    }

    "empty stream for non-exist services" in {
      ServiceLoader[NotThere] shouldBe empty
    }

  }

  "The ServiceLoader" should {

    "find the first (non-erroring) service" in {
      ServiceLoader[Test] map (_.getClass) shouldBe Some(classOf[Test3])
    }
    "None for non-exist services" in {
      ServiceLoader[NotThere] shouldBe None
    }
  }
}
