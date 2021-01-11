package com.srednal.snug

//import scala.annotation.nowarn

//@nowarn("msg=match may not be exhaustive") // extraction tests: val a / b / c = ...
class PathTest extends UnitTest {
  import Path._

  "Path" should {

    "construct a simple path from ^" in {
      ^ / "foo" / "bar" / "baz" shouldBe Path("foo", "bar", "baz")
    }

    "construct a simple path from %" in {
      % / "foo" / "bar" / "baz" shouldBe Path("/foo", "bar", "baz")
    }

    "construct a simple path from pimped string" in {
      "foo" / "bar" shouldBe Path("foo", "bar")
      "foo" / Path("bar", "baz") shouldBe Path("foo", "bar", "baz")
      "/foo" / Path("bar", "baz") shouldBe Path("/foo", "bar", "baz")
    }

    "construct a path from a seq" in {
      Path("foo" :: "bar" :: "baz" :: Nil) shouldBe Path("foo", "bar", "baz")
      Path(Nil) shouldBe ^
      Path("/foo" :: "bar" :: "baz" :: Nil) shouldBe Path("/foo", "bar", "baz")
    }

    "extract a path's name" in {
      Path("foo", "bar", "baz").name shouldBe "baz"
    }

    "extract a path's parent" in {
      Path("foo", "bar", "baz").parent shouldBe Path("foo", "bar")
      Path("foo").parent shouldBe ^
      Path("/foo").parent shouldBe %
    }

    "recognise ^ and % in empty strings" in {
      Path("") shouldBe ^
      Path("/") shouldBe %
      Path() shouldBe ^
    }

    "not have a parent for % or ^" in {
      an[NoSuchElementException] should be thrownBy %.parent
      an[NoSuchElementException] should be thrownBy ^.parent
    }

    "non-empty paths should not be root" in {
      Path("foo") should have(prop"root"(false))
    }

    "Roots should be root" in {
      (^) should have(prop"root"(true))
      (%) should have(prop"root"(true))
      Path("") should have(prop"root"(true))
      Path("/") should have(prop"root"(true))
    }

    "know absolute vs relative paths" in {
      (^) should have(prop"absolute"(false))
      (%) should have(prop"absolute"(true))
      (^ / "foo" / "bar") should have(prop"absolute"(false))
      (% / "foo" / "bar") should have(prop"absolute"(true))
    }

    "convert to absolute" in {
      ^.asAbsolute shouldBe %
      %.asAbsolute shouldBe %
      (^ / "foo" / "bar").asAbsolute shouldBe % / "foo" / "bar"
      (% / "foo" / "bar").asAbsolute shouldBe % / "foo" / "bar"
    }

    "convert to reative" in {
      ^.asRelative shouldBe ^
      %.asRelative shouldBe ^
      (^ / "foo" / "bar").asRelative shouldBe ^ / "foo" / "bar"
      (% / "foo" / "bar").asRelative shouldBe ^ / "foo" / "bar"
    }

    "expand /'s in strings" in {
      "foo/bar/baz".asPath shouldBe Path("foo", "bar", "baz")
      (^ / "foo/bar/baz") shouldBe Path("foo", "bar", "baz")
      (% / "foo/bar/baz") shouldBe Path("/", "foo", "bar", "baz")
      "foo/bar" / "baz" shouldBe Path("foo", "bar", "baz")
      "foo" / "bar/baz" shouldBe Path("foo", "bar", "baz")
    }

    "collapse extra /'s in strings" in {
      "/foo/bar/baz".asPath shouldBe Path("/foo", "bar", "baz")
      "foo/bar/baz/".asPath shouldBe Path("foo", "bar", "baz")
      "foo/bar//baz".asPath shouldBe Path("foo", "bar", "baz")
      "//foo//bar////baz//".asPath shouldBe Path("/foo", "bar", "baz")
    }

    "concat two paths with /" in {
      Path("foo", "bar") / Path("baz", "qux") shouldBe Path("foo", "bar", "baz", "qux")
    }

    "collapse extra roots in paths" in {
      (^ / "foo" / ^ / "/" / "bar" / "" / "baz" / ^) shouldBe Path("foo", "bar", "baz")
      (% / "foo" / ^ / "/" / "bar" / "" / "baz" / ^) shouldBe % / Path("foo", "bar", "baz")
    }

    "not allow % (absolute root) in the middle of a path" in {
      an[IllegalArgumentException] should be thrownBy ^ / "foo" / % / "bar" // root in the middle
    }

    "extract as parent/name (relative)" in {
      ^ / "foo" / "bar" / "baz" match {
        case p / n =>
          p shouldBe ^ / "foo" / "bar"
          n shouldBe "baz"
        case x => fail(s"not matched: $x")
      }
    }

    "extract as parent/sub/name (relative)" in {
      ^ / "foo" / "bar" / "baz" / "qux" match {
        case a / b / c =>
          a shouldBe ^ / "foo" / "bar"
          b shouldBe "baz"
          c shouldBe "qux"
        case x => fail(s"not matched: $x")
      }
    }

    "extract via Path.unapplySeq (relative)" in {
      ^ / "foo" / "bar" / "baz" / "qux" match {
        case Path(a, b, cs@_*) =>
          a shouldBe "foo"
          b shouldBe "bar"
          cs shouldBe "baz" :: "qux" :: Nil
        case x => fail(s"not matched: $x")
      }

      ^ / "foo" / "bar" match {
        case Path(d, e, fs@_*) =>
          d shouldBe "foo"
          e shouldBe "bar"
          fs shouldBe Nil
        case x => fail(s"not matched: $x")
      }
    }

    "match relative path" in {
      ^ / "foo" / "bar" match {
        case % / f / b => fail(s"matched with % / $f / $b")
        case ^ / _ / _ => // pass
        case x => fail(s"not matched $x")
      }
    }

    "match absolute path" in {
      % / "foo" / "bar" match {
        case ^ / f / b => fail(s"matched with ^ / $f / $b")
        case % / _ / _ => // pass
        case x => fail(s"not matched $x")
      }
    }

    "extract as parent/name (absolute)" in {
      % / "foo" / "bar" / "baz" match {
        case p / n =>
          p shouldBe % / "foo" / "bar"
          n shouldBe "baz"
        case x => fail(s"not matched: $x")
      }
    }

    "extract as parent/sub/name (absolute)" in {
      % / "foo" / "bar" / "baz" / "qux" match {
        case a / b / c =>
          a shouldBe % / "foo" / "bar"
          b shouldBe "baz"
          c shouldBe "qux"
        case x => fail(s"not matched: $x")
      }
    }

    "extract via Path.unapplySeq (absolute)" in {
      % / "foo" / "bar" / "baz" / "qux" match {
        case Path(a, b, cs@_*) =>
          a shouldBe "foo"
          b shouldBe "bar"
          cs shouldBe "baz" :: "qux" :: Nil
        case x => fail(s"not matched: $x")
      }

      % / "foo" / "bar" match {
        case Path(d, e, fs@_*) =>
          d shouldBe "foo"
          e shouldBe "bar"
          fs shouldBe Nil
        case x => fail(s"not matched: $x")
      }
    }

    "toString reasonably" in {
      Path("foo", "bar").toString shouldBe "foo/bar"
      (^ / "foo" / "bar").toString shouldBe "foo/bar"
      (% / "foo" / "bar").toString shouldBe "/foo/bar"
      %.toString shouldBe "/"
      ^.toString shouldBe ""
    }

    "equals vs root without bonking (symmetrically)" in {
      Path("foo") should not be ^
      Path("foo") should not be %
      (^) should not be Path("foo")
      (%) should not be Path("foo")
    }
  }
}
