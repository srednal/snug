package com.srednal.snug

import org.scalatest._

class PathTest extends WordSpec with Matchers {
  import Path._

  "Path" should {

    "construct a simple path from ^" in {
      ^ / "foo" / "bar" / "baz" shouldBe Path("foo", "bar", "baz")
    }
    "construct a simple path from pimped string" in {
      "foo" / "bar" shouldBe Path("foo", "bar")
      "foo" / Path("bar", "baz") shouldBe Path("foo", "bar", "baz")
    }
    "construct a path from a seq" in {
      Path("foo" :: "bar" :: "baz" :: Nil)shouldBe Path("foo", "bar", "baz")
      Path(Nil) shouldBe ^
    }
    "extract the path's name" in {
       Path("foo", "bar", "baz").name shouldBe "baz"
    }
    "extract the path's parent" in {
       Path("foo", "bar", "baz").parent shouldBe Path("foo", "bar")
    }
    "recognise ^ in empty strings" in {
      Path("") shouldBe ^
      Path("/") shouldBe ^
    }
    "non-empty paths should not be empty" in {
      Path("foo") should not be empty
    }
    "Root ^ should be empty" in {
      ^ shouldBe empty
      Path("") shouldBe empty
      Path("/") shouldBe empty
    }
    "expand /'s in strings" in {
      "foo/bar/baz".asPath shouldBe Path("foo", "bar", "baz")
      ^ / "foo/bar/baz" shouldBe Path("foo", "bar", "baz")
      "foo/bar" / "baz" shouldBe Path("foo", "bar", "baz")
      "foo" / "bar/baz" shouldBe Path("foo", "bar", "baz")
    }
    "collapse extra /'s in strings" in {
      "/foo/bar/baz".asPath shouldBe Path("foo", "bar", "baz")
      "foo/bar/baz/".asPath shouldBe Path("foo", "bar", "baz")
      "foo/bar//baz".asPath shouldBe Path("foo", "bar", "baz")
      "//foo//bar////baz//".asPath shouldBe Path("foo", "bar", "baz")
    }

    "concat two paths with /" in {
      Path("foo", "bar") / Path("baz", "qux") shouldBe Path("foo", "bar", "baz", "qux")
    }
    "collapse extra roots in paths" in {
      ^ / "foo" / ^ / "/" / "bar" / "" / "baz" / ^ shouldBe Path("foo", "bar", "baz")
    }

    "extract as parent/name" in {
      val p / n = "foo" / "bar" / "baz"
      p shouldBe ^ / "foo" / "bar"
      n shouldBe "baz"
    }
    "extract as parent/sub/name" in {
      val a / b / c = "foo" / "bar" / "baz" / "qux"
      a shouldBe ^ / "foo" / "bar"
      b shouldBe "baz"
      c shouldBe "qux"
    }
    "extract via Path.unapplySeq" in {
      val Path(a, b, cs@_*) = "foo" / "bar" / "baz" / "qux"
      a shouldBe "foo"
      b shouldBe "bar"
      cs shouldBe "baz" :: "qux" :: Nil

      val Path(d, e, fs@_*) = "foo" / "bar"
      d shouldBe "foo"
      e shouldBe "bar"
      fs shouldBe Nil
    }

    "toString reasonably" in {
      Path("foo", "bar").toString shouldBe "/foo/bar"
      ^.toString shouldBe "/"
    }
  }
}
