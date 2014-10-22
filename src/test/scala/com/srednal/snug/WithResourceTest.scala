package com.srednal.snug

import org.scalatest._
import java.io.Closeable
import scala.io.Source

class WithResourceTest extends WordSpec with Matchers {

  "The WithResource" should {

    "run and close with the closer" in {
      var ran = false
      var closed = false
      WithResource.withCloser({closed = true}) {ran = true}
      ran shouldBe true
      closed shouldBe true
    }

    "close even if run pukes" in {
      var closed = false
      an[Exception] should be thrownBy WithResource.withCloser({closed = true}) {throw new Exception}
      closed shouldBe true
    }

    "handle closer exceptions quietly" in {
      var ran = false
      WithResource.withCloser({throw new Exception}) {ran = true}
      ran shouldBe true
    }

    "return the thing" in {
      WithResource.withCloser({}) { "thing" } shouldBe "thing"
    }

    "close a Closeable" in {
      var closed = false
      class C extends Closeable {
        override def close() = { closed = true}
      }
      var ranWith: C = null
      val c = new C
      WithResource.withResource(c) {x => ranWith = x; "thing"} shouldBe "thing"
      ranWith shouldBe c
      closed shouldBe true
    }

    "close a Source" in {
      var closed = false
      class S extends Source {
        override def close() = { closed = true}
        override val iter = Nil.iterator
      }
      var ranWith: S = null
      val s = new S
      WithResource.withSource(s) {x => ranWith = x; "thing"} shouldBe "thing"
      ranWith shouldBe s
      closed shouldBe true
    }
  }
}
