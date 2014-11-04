package com.srednal.snug.config

import akka.util.Timeout
import org.scalatest._
import scala.concurrent.duration._

case class TestConfigHolder(number: Int,
                            pi: Double,
                            interval: Duration,
                            hello: String,
                            yes: Boolean,
                            names: List[String])

class ConfigTest extends WordSpec with Matchers {

  "Config" should {

    "fetch a string" in {
      config.as[String]("foo.hello") shouldBe "World"
    }
    "fetch an int" in {
      config.as[Int]("foo.number") shouldBe 42
    }
    "fetch a long" in {
      config.as[Long]("foo.number") shouldBe 42L
    }
    "fetch a double" in {
      config.as[Double]("foo.pi") shouldBe 3.14
      config.as[Double]("foo.number") shouldBe 42.0
    }
    "fetch a boolean" in {
      config.as[Boolean]("foo.yes") shouldBe true
    }
    "fetch a duration" in {
      config.as[Duration]("foo.interval") shouldBe 5.seconds
      config.as[FiniteDuration]("foo.interval") shouldBe 5.seconds
      config.as[Timeout]("foo.interval") shouldBe Timeout(5.seconds)
      config.as[Duration]("foo.number") shouldBe 42.millis
      config.as[Duration]("foo.pi") shouldBe 3.14.millis
    }
    "fetch a list of strings" in {
      config.as[Seq[String]]("foo.names") shouldBe Seq("foo", "bar", "baz")
      config.as[Set[String]]("foo.names") shouldBe Set("foo", "bar", "baz")
      config.as[Traversable[String]]("foo.names") shouldBe Traversable("foo", "bar", "baz")
      config.as[Iterable[String]]("foo.names") shouldBe Iterable("foo", "bar", "baz")
      config.as[List[String]]("foo.names") shouldBe List("foo", "bar", "baz")
    }

    "fetch a sub-config" in {
      config("foo").as[String]("hello") shouldBe "World"
    }

    "fetch anything as a string" in {
      config.as[String]("foo.hello") shouldBe "World"
      config.as[String]("foo.number") shouldBe "42"
      config.as[String]("foo.pi") shouldBe "3.14"
      config.as[String]("foo.yes") shouldBe "true"
      config.as[String]("foo.interval") shouldBe "5 seconds"
      val f = config.as[String]("foo")
      f should {
        startWith("{") and
          endWith("}") and
          include("hello=World") and
          include("yes=true")
      }
    }

    "fetch as case class" in {
      config.as[TestConfigHolder]("foo") shouldBe
        TestConfigHolder(
          number = 42,
          pi = 3.14,
          interval = 5.seconds,
          hello = "World",
          yes = true,
          names = "foo" :: "bar" :: "baz" :: Nil)
    }

    "fetch as options" in {
      config.as[Option[String]]("foo.hello") shouldBe Some("World")
      config.as[Option[Double]]("foo.pi") shouldBe Some(3.14)
      config.as[Option[List[String]]]("foo.names") shouldBe Some(List("foo", "bar", "baz"))

      config.as[Option[TestConfigHolder]]("foo") shouldBe Some(
        TestConfigHolder(
          number = 42,
          pi = 3.14,
          interval = 5.seconds,
          hello = "World",
          yes = true,
          names = "foo" :: "bar" :: "baz" :: Nil))

      config.as[Option[String]]("not.there") shouldBe None
      config.as[Option[TestConfigHolder]]("bar") shouldBe None
      config.as[Option[List[String]]]("baz") shouldBe None
    }

    "error in various ways" in {
      // note that simply calling config.as[Int] will not error, as the Int is not unboxed until it is assigned a type
      a[ClassCastException] should be thrownBy {
        val i = config.as[Int]("foo.hello")
      }
      a[ClassCastException] should be thrownBy {
        config.as[TestConfigHolder]("foo.hello")
      }
    }
  }
}
