package com.srednal.snug.config

import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.config.ConfigException.WrongType
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
      config[String]("foo.hello") shouldBe "World"
    }
    "fetch an int" in {
      config[Int]("foo.number") shouldBe 42
    }
    "fetch a long" in {
      config[Long]("foo.number") shouldBe 42L
    }
    "fetch a double" in {
      config[Double]("foo.pi") shouldBe 3.14
      config[Double]("foo.number") shouldBe 42.0
    }
    "fetch a boolean" in {
      config[Boolean]("foo.yes") shouldBe true
    }
    "fetch a duration" in {
      config[Duration]("foo.interval") shouldBe 5.seconds
      config[FiniteDuration]("foo.interval") shouldBe 5.seconds
      config[Timeout]("foo.interval") shouldBe Timeout(5.seconds)
      config[Duration]("foo.number") shouldBe 42.millis
      config[Duration]("foo.pi") shouldBe 3.14.millis
    }
    "fetch a list of strings" in {
      config[Seq[String]]("foo.names") shouldBe Seq("foo", "bar", "baz")
      config[Set[String]]("foo.names") shouldBe Set("foo", "bar", "baz")
      config[Traversable[String]]("foo.names") shouldBe Traversable("foo", "bar", "baz")
      config[Iterable[String]]("foo.names") shouldBe Iterable("foo", "bar", "baz")
      config[List[String]]("foo.names") shouldBe List("foo", "bar", "baz")
    }

    "fetch a sub-config" in {
      val sc: Config = config[Config]("foo")
      sc[String]("hello") shouldBe "World"
    }

    "fetch anything as a string" in {
      config[String]("foo.hello") shouldBe "World"
      config[String]("foo.number") shouldBe "42"
      config[String]("foo.pi") shouldBe "3.14"
      config[String]("foo.yes") shouldBe "true"
      config[String]("foo.interval") shouldBe "5 seconds"
      val f: String = config("foo")
      f should {
        startWith("{") and
          endWith("}") and
          include("hello=World") and
          include("yes=true")
      }
    }


    implicit object TestConfigHolderCvt extends ConfigConversion[TestConfigHolder] {
      def apply(cfg: Config, path: String) = {
        val c: Config = cfg(path)
        TestConfigHolder(
          c("number"),
          c("pi"),
          c("interval"),
          c("hello"),
          c("yes"),
          c("names")
        )
      }
    }

    "fetch as case class" in {
      config[TestConfigHolder]("foo") shouldBe
        TestConfigHolder(
          number = 42,
          pi = 3.14,
          interval = 5.seconds,
          hello = "World",
          yes = true,
          names = "foo" :: "bar" :: "baz" :: Nil)
    }

    "fetch as options" in {
      config[Option[String]]("foo.hello") shouldBe Some("World")
      config[Option[Double]]("foo.pi") shouldBe Some(3.14)
      config[Option[List[String]]]("foo.names") shouldBe Some(List("foo", "bar", "baz"))

      config[Option[TestConfigHolder]]("foo") shouldBe Some(
        TestConfigHolder(
          number = 42,
          pi = 3.14,
          interval = 5.seconds,
          hello = "World",
          yes = true,
          names = "foo" :: "bar" :: "baz" :: Nil))

      config[Option[String]]("not.there") shouldBe None
      config[Option[TestConfigHolder]]("bar") shouldBe None
      config[Option[List[String]]]("baz") shouldBe None
    }

    "error in various ways" in {
      a[WrongType] should be thrownBy {
        config[Int]("foo.hello")
      }
      a[WrongType] should be thrownBy {
        config[TestConfigHolder]("foo.hello")
      }
    }
  }
}
