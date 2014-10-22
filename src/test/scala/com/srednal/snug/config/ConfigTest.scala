package com.srednal.snug.config

import org.scalatest._
import scala.concurrent.duration._

class ConfigTest extends WordSpec with Matchers {

  "Config" should {

    "fetch a string" in {
      config.string("foo.hello") shouldBe "World"
    }
    "fetch an int" in {
      config.int("foo.number") shouldBe 42
    }
    "fetch a long" in {
      config.long("foo.number") shouldBe 42L
    }
    "fetch a double" in {
      config.double("foo.pi") shouldBe 3.14
      config.double("foo.number") shouldBe 42.0
    }
    "fetch a boolean" in {
      config.boolean("foo.yes") shouldBe true
    }
    "fetch a duration" in {
      config.duration("foo.interval") shouldBe 5.seconds
    }
    "fetch a seq of strings" in {
      config.seqString("foo.names") shouldBe "foo" :: "bar" :: "baz" :: Nil
    }

    "fetch a sub-config" in {
      config("foo").string("hello") shouldBe "World"
    }

    "fetch anything as a string" in {
      config.string("foo.number") shouldBe "42"
      config.string("foo.pi") shouldBe "3.14"
      config.string("foo.yes") shouldBe "true"
      config.string("foo.interval") shouldBe "5 seconds"
    }
  }

  "The Config string interpolation" should {
    "fetch a string" in {
      cstr"foo.hello" shouldBe "World"
    }
    "fetch an int" in {
      cint"foo.number" shouldBe 42
    }
    "fetch a long" in {
      clong"foo.number" shouldBe 42L
    }
    "fetch a double" in {
      cdbl"foo.pi" shouldBe 3.14
      cdbl"foo.number" shouldBe 42.0
    }
    "fetch a boolean" in {
      cbool"foo.yes" shouldBe true
    }
    "fetch a duration" in {
      cdur"foo.interval" shouldBe 5.seconds
    }

  }
}
