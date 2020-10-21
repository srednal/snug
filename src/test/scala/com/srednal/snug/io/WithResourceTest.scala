package com.srednal.snug
package io

import java.io.Closeable
import scala.io.Source

class WithResourceTest extends UnitTest {

  "The WithResource" should {

    "run and close with the closer" in {
      var ran = false
      var closed = false
      WithResource.withCloser{closed = true} {ran = true}
      ran shouldBe true
      closed shouldBe true
    }

    "close even if run pukes" in {
      var closed = false
      def closeit(): Unit = closed = true
      an[Exception] should be thrownBy WithResource.withCloser(closeit()) {throw new Exception}
      closed shouldBe true
    }

    "handle closer exceptions quietly" in {
      var ran = false
      WithResource.withCloser{throw new Exception} {ran = true}
      ran shouldBe true
    }

    "return the thing" in {
      WithResource.withCloser{} { "thing" } shouldBe "thing"
    }

    "close a Closeable" in {
      var closed = false
      class C extends Closeable {
        override def close(): Unit = { closed = true}
      }
      var ranWith: Option[C] = None
      val c = new C
      WithResource.withResource(c) {x => ranWith = Some(x); "thing"} shouldBe "thing"
      ranWith shouldBe Some(c)
      closed shouldBe true
    }

    "close a Source" in {
      var closed = false
      class S extends Source {
        override def close(): Unit = { closed = true}
        override val iter = Nil.iterator
      }
      var ranWith: Option[S] = None
      val s = new S
      WithResource.withSource(s) {x => ranWith = Some(x); "thing"} shouldBe "thing"
      ranWith shouldBe Some(s)
      closed shouldBe true
    }
  }
}
