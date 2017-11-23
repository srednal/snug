package com.srednal.snug.debug

import com.typesafe.scalalogging.Logger
import org.mockito.ArgumentCaptor
import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito._
object DebugTest {
  val w = 42

  def captor[X: Manifest]: ArgumentCaptor[X] = ArgumentCaptor.forClass[X, X](manifest[X].runtimeClass.asInstanceOf[Class[X]])
}

class DebugTest extends WordSpec with MockitoSugar with Matchers with DebugLog with BeforeAndAfterEach {
  import DebugTest._

  val underlying = mock[org.slf4j.Logger]
  override val logger = Logger(underlying)

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(underlying)
    when(underlying.isDebugEnabled).thenReturn(true)
    when(underlying.isTraceEnabled).thenReturn(true)
    ()
  }

  val x = "the value"
  def z = 3.14
  def zz(s: String) = s.reverse

  "the debug macro" should {

    "debug a single constant" in {
      debuglog("foo")
      verify(underlying).debug("foo")
    }

    "debug a single non-string constant" in {
      debuglog(123)
      verify(underlying).debug("123")
    }

    "debug a local val" in {
      val foo = "xyzzy"
      debuglog(foo)
      verify(underlying).debug("foo = xyzzy")
    }
    "debug a local var" in {
      var foo: String = ""
      foo = "xyzzy"
      debuglog(foo)
      verify(underlying).debug("foo = xyzzy")
    }

    "debug a field" in {
      debuglog(x)
      verify(underlying).debug("DebugTest.this.x = the value")
    }

    "debug a noarg method" in {
      debuglog(z)
      verify(underlying).debug("DebugTest.this.z = 3.14")
    }
    "debug a method" in {
      debuglog(zz("foo"))
      verify(underlying).debug( """DebugTest.this.zz("foo") = oof""")
    }

    "debug a function defn reference" in {
      val f: String => Int = _.toInt
      debuglog(f)
      val c = captor[String]
      verify(underlying).debug(c.capture)
      c.getValue should startWith("f = ")
    }
    "debug a function invocation" in {
      val f: String => Int = _.toInt
      debuglog(f("13"))
      verify(underlying).debug( """f.apply("13") = 13""")
    }
    "debug a companion object field" in {
      debuglog(w)
      verify(underlying).debug("DebugTest.w = 42")
    }
    "debug several args as multiple logger calls" in {
      debuglog(w, x, z)
      verify(underlying).debug("DebugTest.w = 42")
      verify(underlying).debug("DebugTest.this.x = the value")
      verify(underlying).debug("DebugTest.this.z = 3.14")
    }
  }

  "the trace macro" should {

    "trace a single constant" in {
      tracelog("foo")
      verify(underlying).trace("foo")
    }

    "trace a local val" in {
      val foo = "xyzzy"
      tracelog(foo)
      verify(underlying).trace("foo = xyzzy")
    }
    "trace a local var" in {
      var foo: String = ""
      foo = "xyzzy"
      tracelog(foo)
      verify(underlying).trace("foo = xyzzy")
    }

    "trace a field" in {
      tracelog(x)
      verify(underlying).trace("DebugTest.this.x = the value")
    }

    "trace a method" in {
      tracelog(z)
      verify(underlying).trace("DebugTest.this.z = 3.14")
    }
    "trace a companion object field" in {
      tracelog(w)
      verify(underlying).trace("DebugTest.w = 42")
    }
    "trace several args as multiple logger calls" in {
      tracelog(w, x, z)
      verify(underlying).trace(
        "DebugTest.w = 42")
      verify(underlying).trace("DebugTest.this.x = the value")
      verify(underlying).trace("DebugTest.this.z = 3.14")
    }
  }
}
