package com.srednal.snug

import com.srednal.snug.log.Logger
import org.scalatest._
import scala.collection.mutable

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
      Test.counts shouldBe Array(0, 0, 1)  // does initially instantiate head

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

    "log errors to implicit logger" in {
      val errors = mutable.Buffer[Option[Throwable]]()
      implicit val testLogger = new Logger {
        override def error(m: => String, t: Option[Throwable]) = errors += t
        override def trace(m: => String, t: Option[Throwable]) = ???
        override def debug(m: => String, t: Option[Throwable]) = ???
        override def info(m: => String, t: Option[Throwable]) = ???
        override def warn(m: => String, t: Option[Throwable]) = ???
      }

      ServicesLoader[Test].toStream.force

      errors should have ('size(5))
      errors(0).get.getMessage should include ("Provider com.srednal.snug.testservices.TestErr1 could not be instantiated")
      errors(0).get.getCause.getMessage should include("from TestErr1")
      errors(1).get.getMessage should include ("Provider com.srednal.snug.testservices.NoneSuch not found")
      errors(2).get.getMessage should include ("Provider com.srednal.snug.testservices.TestErr2 could not be instantiated")
      errors(2).get.getCause.getMessage should include("from TestErr2")
      errors(3).get.getMessage should include ("Provider com.srednal.snug.testservices.Test could not be instantiated")
      errors(3).get.getCause shouldBe a[InstantiationException]
      errors(4).get.getMessage should include ("Provider com.srednal.snug.testservices.TestErr3 could not be instantiated")
      errors(4).get.getCause.getMessage should include("from TestErr3")
    }
  }

  "The ServiceLoader" should {

    "find the first (non-erroring) service" in {
      ServiceLoader[Test] map (_.getClass) shouldBe Some(classOf[Test3])
    }
    "None for non-exist services" in {
      ServiceLoader[NotThere] shouldBe None
    }

    "log errors to implicit logger" in {
      val errors = mutable.Buffer[Option[Throwable]]()
      implicit val testLogger = new Logger {
        override def error(m: => String, t: Option[Throwable]) = errors += t
        override def trace(m: => String, t: Option[Throwable]) = ???
        override def debug(m: => String, t: Option[Throwable]) = ???
        override def info(m: => String, t: Option[Throwable]) = ???
        override def warn(m: => String, t: Option[Throwable]) = ???
      }

      ServiceLoader[Test]

      errors should have ('size(1))
      errors(0).get.getMessage should include ("Provider com.srednal.snug.testservices.TestErr1 could not be instantiated")
    }
  }
}
