package com.srednal.snug
package config

import scala.concurrent.duration._
import java.time.{Duration => JDuration}
import scala.util.{Success, Try}
import java.net.{InetAddress, InetSocketAddress, URI, URL}
import com.typesafe.config.ConfigException.WrongType
import com.typesafe.config.{Config, ConfigFactory}

case class TestConfigHolder(number: Int,
                            pi: Double,
                            interval: Duration,
                            hello: String,
                            yes: Boolean,
                            names: Seq[String])

class ConfigTest extends UnitTest {

  "Config" should {

    // See src/test/resources/application.conf

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

    "fetch a float" in {
      config[Float]("foo.pi") shouldBe 3.14f
      config[Float]("foo.number") shouldBe 42.0f
    }

    "fetch a boolean" in {
      config[Boolean]("foo.yes") shouldBe true
    }

    "fetch a duration" in {
      config[JDuration]("foo.interval") shouldBe JDuration.ofSeconds(5)
      config[Duration]("foo.interval") shouldBe 5.seconds
      config[FiniteDuration]("foo.interval") shouldBe 5.seconds
      config[Duration]("foo.number") shouldBe 42.millis
      config[Duration]("foo.pi") shouldBe 3.14.millis
    }

    "fetch a collection of strings" in {
      config[Seq[String]]("foo.names") shouldBe Seq("foo", "bar", "baz", "foo")
      config[Iterable[String]]("foo.names") shouldBe Iterable("foo", "bar", "baz", "foo")
      config[List[String]]("foo.names") shouldBe List("foo", "bar", "baz", "foo")

      // sets have unique values
      config[Set[String]]("foo.names") shouldBe Set("foo", "bar", "baz")
    }

    "fetch a sub-config" in {
      val sc: Config = config[Config]("foo")
      sc[String]("hello") shouldBe "World"
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

    val failure: HavePropertyMatcherGenerator = Symbol("failure")

    "fetch as Try" in {
      config[Try[String]]("foo.hello") shouldBe Success("World")
      config[Try[Double]]("foo.pi") shouldBe Success(3.14)
      config[Try[Int]]("foo.hello") should have(failure(true))
    }

    "fetch URI and URL" in {
      ConfigFactory.parseString( """myUri: "http://example.com/foo" """)[URI]("myUri") shouldBe new URI("http://example.com/foo")
      ConfigFactory.parseString( """myUrl: "http://example.com/foo" """)[URL]("myUrl") shouldBe new URL("http://example.com/foo")
    }

    "fetch as Path" in {
      val c = ConfigFactory.parseString(
        """
        a: "a"
        babs: "/b"
        abc: "/a/b/c"
        abcrel: "a/b/c"
        """)

      c[Path]("a") shouldBe Path("a")
      c[Path]("babs") shouldBe Path("/b")
      c[Path]("abc") shouldBe Path("/a/b/c")
      c[Path]("abcrel") shouldBe Path("a/b/c")
    }

    "fetch InetAddress" in {
      val c = ConfigFactory.parseString(
        """
        srednal: srednal.com
        local: localhost
        loop: "127.0.0.1"
        gdns: "8.8.8.8"
        ipv6loop: "::1"
        ipv6: "2001:4860:4860::8888"
        """)

      c[InetAddress]("srednal") shouldBe InetAddress.getByName("srednal.com")
      c[InetAddress]("local") shouldBe InetAddress.getByName("localhost")
      c[InetAddress]("loop") shouldBe InetAddress.getByName("127.0.0.1")
      c[InetAddress]("gdns") shouldBe InetAddress.getByName("8.8.8.8")
      c[InetAddress]("ipv6loop") shouldBe InetAddress.getByName("::1")
      c[InetAddress]("ipv6") shouldBe InetAddress.getByName("2001:4860:4860::8888")
    }

    "fetch InetSocketAddress from host:port" in {
      val c = ConfigFactory.parseString(
        """
        srednal: "srednal.com"
        srednal80: "srednal.com:80"
        local: localhost
        local80: "localhost:80"
        loop: "127.0.0.1"
        loop80: "127.0.0.1:80"
        justPort: 8800
        colonPort: ":123"
        hostColon: "hostname:"
        ipv6: "2001:4860:4860::8888:"  // requires trailing colon to interpret whole thing as host
        ipv680: "2001:4860:4860::8888:80"
        empty: ""
        """)

      c[InetSocketAddress]("srednal") shouldBe new InetSocketAddress("srednal.com", 0)
      c[InetSocketAddress]("srednal80") shouldBe new InetSocketAddress("srednal.com", 80)
      c[InetSocketAddress]("local") shouldBe new InetSocketAddress("localhost", 0)
      c[InetSocketAddress]("local80") shouldBe new InetSocketAddress("localhost", 80)
      c[InetSocketAddress]("loop") shouldBe new InetSocketAddress("127.0.0.1", 0)
      c[InetSocketAddress]("loop80") shouldBe new InetSocketAddress("127.0.0.1", 80)
      c[InetSocketAddress]("justPort") shouldBe new InetSocketAddress(8800)
      c[InetSocketAddress]("colonPort") shouldBe new InetSocketAddress(123)
      c[InetSocketAddress]("hostColon") shouldBe new InetSocketAddress("hostname", 0)
      c[InetSocketAddress]("ipv6") shouldBe new InetSocketAddress("2001:4860:4860::8888", 0)
      c[InetSocketAddress]("ipv680") shouldBe new InetSocketAddress("2001:4860:4860::8888", 80)
      c[InetSocketAddress]("empty") shouldBe new InetSocketAddress(0)
    }

    "error in reasonable ways" in {
      a[WrongType] should be thrownBy {
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
      val setKey = ConfigKey[Set[String]]("foo.names")
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

    "fetch a Tuple" in {
      config[(String, Double)]("foo.tuple2").shouldBe(("foo", 3.14))
      config[(String, Int, Duration)]("foo.tuple3").shouldBe(("foo", 42, 5.seconds))
    }
  }
}
