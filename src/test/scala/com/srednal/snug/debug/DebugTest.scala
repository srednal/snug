package com.srednal.snug.debug

import scala.collection.mutable.ListBuffer
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

class DebugTest extends WordSpec with Matchers with DebugLog with BeforeAndAfterEach {
  import DebugTest._

  override val logger = new X
  override def beforeEach(): Unit = {
    logger.debugged.clear()
    logger.traced.clear()
    super.beforeEach()
  }

  val x = "the value"
  def z = 3.14
  def zz(s: String) = s.reverse

  "the debug macro" should {

    "debug a single constant" in {
      debuglog("foo")
      logger.debugged should contain only "foo"
    }

    "debug a single non-string constant" in {
      debuglog(123)
      logger.debugged should contain only "123"
    }

    "debug a local val" in {
      val foo = "xyzzy"
      debuglog(foo)
      logger.debugged should contain only "foo = xyzzy"
    }
    "debug a local var" in {
      var foo: String = ""
      foo = "xyzzy"
      debuglog(foo)
      logger.debugged should contain only "foo = xyzzy"
    }

    "debug a field" in {
      debuglog(x)
      logger.debugged should contain only "DebugTest.this.x = the value"
    }

    "debug a noarg method" in {
      debuglog(z)
      logger.debugged should contain only "DebugTest.this.z = 3.14"
    }
    "debug a method" in {
      debuglog(zz("foo"))
      logger.debugged should contain only """DebugTest.this.zz("foo") = oof"""
    }

    "debug a function defn reference" in {
      val f: String => Int = _.toInt
      debuglog(f)
      logger.debugged.head should startWith
      "f = "
    }
    "debug a function invocation" in {
      val f: String => Int = _.toInt
      debuglog(f("13"))
      logger.debugged should contain only """f.apply("13") = 13"""
    }
    "debug a companion object field" in {
      debuglog(w)
      logger.debugged should contain only "DebugTest.w = 42"
    }
    "debug several args as multiple logger calls" in {
      debuglog(w, x, z)
      logger.debugged should contain inOrder(
        "DebugTest.w = 42",
        "DebugTest.this.x = the value",
        "DebugTest.this.z = 3.14"
      )
    }
  }

  "the trace macro" should {

    "trace a single constant" in {
      tracelog("foo")
      logger.traced should contain only "foo"
    }

    "trace a local val" in {
      val foo = "xyzzy"
      tracelog(foo)
      logger.traced should contain only "foo = xyzzy"
    }
    "trace a local var" in {
      logger.traced.clear()
      var foo: String = ""
      foo = "xyzzy"
      tracelog(foo)
      logger.traced should contain only "foo = xyzzy"
    }

    "trace a field" in {
      tracelog(x)
      logger.traced should contain only "DebugTest.this.x = the value"
    }

    "trace a method" in {
      tracelog(z)
      logger.traced should contain only "DebugTest.this.z = 3.14"
    }
    "trace a companion object field" in {
      tracelog(w)
      logger.traced should contain only "DebugTest.w = 42"
    }
    "trace several args as multiple logger calls" in {
      tracelog(w, x, z)
      logger.traced should contain inOrder(
        "DebugTest.w = 42",
        "DebugTest.this.x = the value",
        "DebugTest.this.z = 3.14"
      )
    }
  }
}
