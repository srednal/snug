package com.srednal.snug.debug

import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.{StrictLogging, LazyLogging}
import org.scalatest._

// scalastyle:off magic.number multiple.string.literals import.grouping var.field
object DebugTest {
  val w = 42

  class X {
    val debugged = ListBuffer[String]()
    val traced = ListBuffer[String]()
    def debug(s: String): Unit = debugged += s
    def trace(s: String): Unit = traced += s
  }

}

class DebugTest extends WordSpec with Matchers with DebugLog {
  import DebugTest._

  override val logger = new X

  val x = "the value"
  def z = 3.14
  def zz(s: String) = s.reverse

  "the debug macro" should {

    "debug a single constant" in {
      logger.debugged.clear()
      debuglog("foo")
      logger.debugged shouldBe "foo" :: Nil
    }

    "debug a single non-string constant" in {
      logger.debugged.clear()
      debuglog(123)
      logger.debugged shouldBe "123" :: Nil
    }

    "debug a local val" in {
      logger.debugged.clear()
      val foo = "xyzzy"
      debuglog(foo)
      logger.debugged shouldBe "foo = xyzzy" :: Nil
    }
    "debug a local var" in {
      logger.debugged.clear()
      var foo = "xyzzy"
      debuglog(foo)
      logger.debugged shouldBe "foo = xyzzy" :: Nil
    }

    "debug a field" in {
      logger.debugged.clear()
      debuglog(x)
      logger.debugged shouldBe "DebugTest.this.x = the value" :: Nil
    }

    "debug a method" in {
      logger.debugged.clear()
      debuglog(z)
      debuglog(zz("foo"))
      logger.debugged shouldBe "DebugTest.this.z = 3.14" :: """DebugTest.this.zz("foo") = oof""" :: Nil
    }
    "debug a function" in {
      logger.debugged.clear()
      val f: String => Int = _.toInt
      debuglog(f)
      debuglog(f("13"))
      logger.debugged shouldBe "f = <function1>" :: """f.apply("13") = 13""" :: Nil
    }
    "debug a companion object field" in {
      logger.debugged.clear()
      debuglog(w)
      logger.debugged shouldBe "DebugTest.w = 42" :: Nil
    }
    "debug several args as multiple logger calls" in {
      logger.debugged.clear()
      debuglog(w, x, z)
      logger.debugged shouldBe "DebugTest.w = 42" :: "DebugTest.this.x = the value" :: "DebugTest.this.z = 3.14" :: Nil
    }
  }

  "the trace macro" should {

    "trace a single constant" in {
      logger.traced.clear()
      tracelog("foo")
      logger.traced shouldBe "foo" :: Nil
    }

    "trace a local val" in {
      logger.traced.clear()
      val foo = "xyzzy"
      tracelog(foo)
      logger.traced shouldBe "foo = xyzzy" :: Nil
    }
    "trace a local var" in {
      logger.traced.clear()
      var foo = "xyzzy"
      tracelog(foo)
      logger.traced shouldBe "foo = xyzzy" :: Nil
    }

    "trace a field" in {
      logger.traced.clear()
      tracelog(x)
      logger.traced shouldBe "DebugTest.this.x = the value" :: Nil
    }

    "trace a method" in {
      logger.traced.clear()
      tracelog(z)
      logger.traced shouldBe "DebugTest.this.z = 3.14" :: Nil
    }
    "trace a companion object field" in {
      logger.traced.clear()
      tracelog(w)
      logger.traced shouldBe "DebugTest.w = 42" :: Nil
    }
    "trace several args as multiple logger calls" in {
      logger.traced.clear()
      tracelog(w, x, z)
      logger.traced shouldBe "DebugTest.w = 42" :: "DebugTest.this.x = the value" :: "DebugTest.this.z = 3.14" :: Nil
    }
  }


  "Debug" should {
    "mixin with LazyLogging" in {
      class Test extends LazyLogging with DebugLog
    }
    "mixin with StructLogging" in {
      class Test extends StrictLogging with DebugLog
    }
  }
}
