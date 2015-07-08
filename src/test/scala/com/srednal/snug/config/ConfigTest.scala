package com.srednal.snug.config

import java.net.{InetSocketAddress, InetAddress, URL, URI}

import akka.util.Timeout
import com.srednal.snug.ByteSize
import com.srednal.snug.ByteSize._
import com.typesafe.config.{ConfigFactory, Config}
import com.typesafe.config.ConfigException.WrongType
import org.scalatest._
import scala.concurrent.duration._
import scala.util.{Try, Success}

// IMO being explicit (fewer constants) is good in tests, import scoping is intentional
// scalastyle:off magic.number multiple.string.literals import.grouping

case class TestConfigHolder(number: Int,
                            pi: Double,
                            interval: Duration,
                            hello: String,
                            yes: Boolean,
                            names: Seq[String])

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
    "fetch a BigInt" in {
      config[BigInt]("foo.number") shouldBe BigInt(42)
    }
    "fetch a double" in {
      config[Double]("foo.pi") shouldBe 3.14
      config[Double]("foo.number") shouldBe 42.0
    }
    "fetch a float" in {
      config[Float]("foo.pi") shouldBe 3.14f
      config[Float]("foo.number") shouldBe 42.0f
    }
    "fetch a BigDecimal" in {
      config[BigDecimal]("foo.pi") shouldBe BigDecimal("3.14")
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
    "fetch a byte size" in {
      config[ByteSize]("foo.number") shouldBe 42.B
      config[ByteSize]("foo.size1") shouldBe 123.MB
      config[ByteSize]("foo.size2") shouldBe 1024.KiB
      config[ByteSize]("foo.size3") shouldBe 1.5.TB
    }
    "fetch a collection of strings" in {
      config[Seq[String]]("foo.names") shouldBe Seq("foo", "bar", "baz", "foo")
      config[Traversable[String]]("foo.names") shouldBe Traversable("foo", "bar", "baz", "foo")
      config[Iterable[String]]("foo.names") shouldBe Iterable("foo", "bar", "baz", "foo")
      config[List[String]]("foo.names") shouldBe List("foo", "bar", "baz", "foo")

      // sets have unique values
      config[Set[String]]("foo.names") shouldBe Set("foo", "bar", "baz")
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
      def get(cfg: Config, path: String) = {
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

    "fetch with a custom conversion" in {
      config[TestConfigHolder]("foo") shouldBe
        TestConfigHolder(
          number = 42,
          pi = 3.14,
          interval = 5.seconds,
          hello = "World",
          yes = true,
          names = "foo" :: "bar" :: "baz" :: "foo" :: Nil)
    }

    "fetch as Option" in {
      config[Option[String]]("foo.hello") shouldBe Some("World")
      config[Option[Double]]("foo.pi") shouldBe Some(3.14)
      config[Option[List[String]]]("foo.names") shouldBe Some(List("foo", "bar", "baz", "foo"))

      config[Option[TestConfigHolder]]("foo") shouldBe Some(
        TestConfigHolder(
          number = 42,
          pi = 3.14,
          interval = 5.seconds,
          hello = "World",
          yes = true,
          names = "foo" :: "bar" :: "baz" :: "foo" :: Nil))

      config[Option[String]]("not.there") shouldBe None
      config[Option[TestConfigHolder]]("bar") shouldBe None
      config[Option[List[String]]]("baz") shouldBe None
    }

    "fetch as Try" in {
      config[Try[String]]("foo.hello") shouldBe Success("World")
      config[Try[Double]]("foo.pi") shouldBe Success(3.14)
      config[Try[Int]]("foo.hello") should have('failure(true))
    }

    "fetch URI and URL" in {
      ConfigFactory.parseString( """myUri: "http://example.com/foo" """)[URI]("myUri") shouldBe new URI("http://example.com/foo")
      ConfigFactory.parseString( """myUrl: "http://example.com/foo" """)[URL]("myUrl") shouldBe new URL("http://example.com/foo")
    }

    "fetch InetAddress" in {
      val c = ConfigFactory.parseString(
        """
        srednal: srednal.com
        local: localhost
        loop: "127.0.0.1"
        gdns: "8.8.8.8"
        """)

      c[InetAddress]("srednal") shouldBe InetAddress.getByName("srednal.com")
      c[InetAddress]("local") shouldBe InetAddress.getByName("localhost")
      c[InetAddress]("loop") shouldBe InetAddress.getByName("127.0.0.1")
      c[InetAddress]("gdns") shouldBe InetAddress.getByName("8.8.8.8")
    }

    "fetch InetSocketAddress from host:port" in {
      val c = ConfigFactory.parseString(
        """
        hostPort: "srednal.com:80"
        justPort: 8800
        colonPort: ":123"
        noPort: "example.com:nowhere"
        noColon: foobar
        """)

      c[InetSocketAddress]("hostPort") shouldBe new InetSocketAddress("srednal.com", 80)
      c[InetSocketAddress]("justPort") shouldBe new InetSocketAddress(8800)
      a[WrongType] should be thrownBy c[InetSocketAddress]("noPort")
      a[WrongType] should be thrownBy c[InetSocketAddress]("noColon")
    }

    "error in reasonable ways" in {
      a[NumberFormatException] should be thrownBy {
        config[Int]("foo.hello")
      }
      a[WrongType] should be thrownBy {
        config[TestConfigHolder]("foo.hello")
      }
    }

    "fetch via a ConfigKey[String]" in {
      val helloKey = ConfigKey[String]("foo.hello")
      val hello = config(helloKey)
      hello shouldBe "World"
    }

    "fetch via a ConfigKey for a collection of strings" in {
      val seqKey = ConfigKey[Seq[String]]("foo.names")
      val setKey =   ConfigKey[Set[String]]("foo.names")
      config(seqKey) shouldBe Seq("foo", "bar", "baz", "foo")
      config(setKey) shouldBe Set("foo", "bar", "baz")
    }

    object TestConfigHolderKey extends ConfigKey[TestConfigHolder]("foo")

    "fetch via a ConfigKey object" in {
      config(TestConfigHolderKey) shouldBe
        TestConfigHolder(
          number = 42,
          pi = 3.14,
          interval = 5.seconds,
          hello = "World",
          yes = true,
          names = "foo" :: "bar" :: "baz" :: "foo" :: Nil)
    }
  }
}
