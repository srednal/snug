package com.srednal.snug.log

import org.scalatest._
import scala.collection.mutable

class TestLogger(val name: String) extends Logger {
  val out = mutable.Buffer[(String, String, Option[Throwable])]()
  override def trace(m: => String, t: Option[Throwable]) = out += (("t", m, t))
  override def debug(m: => String, t: Option[Throwable]) = out += (("d", m, t))
  override def info(m: => String, t: Option[Throwable]) = out += (("i", m, t))
  override def warn(m: => String, t: Option[Throwable]) = out += (("w", m, t))
  override def error(m: => String, t: Option[Throwable]) = out += (("e", m, t))
}

object LoggerTest {
  val lm = new LoggerMaker {
    override val factory = new TestLogger(_)
  }
  val testObjectLogger = lm(this)
}

class LoggerTest extends WordSpec with Matchers {
  import LoggerTest._

  "The LoggerMaker" should {
    "create a logger for a name" in {
      lm("foo.bar") should have('name("foo.bar"))
    }
    "create a logger for a class" in {
      lm(this) should have('name("com.srednal.snug.log.LoggerTest"))
    }
    "create a logger for an object" in {
      testObjectLogger should have('name("com.srednal.snug.log.LoggerTest"))
    }
    "create a root logger with empty name" in {
      lm("") should have('name(""))
      lm(null) should have('name(""))
    }
    "use slf4j by default" in {
      Logger(this) shouldBe a[Slf4jLogger]
    }
  }

  "a Logger" should {
    "allow messages to be lazy" in {
      var evaluated: String = ""

      Logger.Silent.trace({evaluated = "trace"; "omg"})
      Logger.Silent.trace({evaluated = "trace1"; "omg"}, new Throwable)
      Logger.Silent.trace({evaluated = "trace2"; this})

      Logger.Silent.debug({evaluated = "debug"; "omg"})
      Logger.Silent.debug({evaluated = "debug1"; "omg"}, new Throwable)
      Logger.Silent.debug({evaluated = "debug2"; this})

      Logger.Silent.info({evaluated = "info"; "omg"})
      Logger.Silent.info({evaluated = "info1"; "omg"}, new Throwable)

      Logger.Silent.warn({evaluated = "warn"; "omg"})
      Logger.Silent.warn({evaluated = "warn1"; "omg"}, new Throwable)

      Logger.Silent.error({evaluated = "error"; "omg"})
      Logger.Silent.error({evaluated = "error1"; "omg"}, new Throwable)

      evaluated shouldBe ""
    }

    "log trace" in {
      val l = lm("tracer")
      val t = new Throwable
      l.trace("foo")
      l.trace("bar", t)
      l.trace(this)
      l.tracing{
        "this is the stuff"
      } shouldBe "this is the stuff"
      l should have('out(Seq(("t", "foo", None), ("t", "bar", Some(t)), ("t", this.toString(), None),("t", "this is the stuff", None))))
    }
    "log debug" in {
      val l = lm("debuger")
      val t = new Throwable
      l.debug("foo")
      l.debug("bar", t)
      l.debug(this)
      l.debuging{
        "this is the stuff"
      } shouldBe "this is the stuff"
      l should have('out(Seq(("d", "foo", None), ("d", "bar", Some(t)), ("d", this.toString(), None),("d", "this is the stuff", None))))
    }
    "log info" in {
      val l = lm("infoer")
      val t = new Throwable
      l.info("foo")
      l.info("bar", t)
      l should have('out(Seq(("i", "foo", None), ("i", "bar", Some(t)))))
    }
    "log warn" in {
      val l = lm("warner")
      val t = new Throwable
      l.warn("foo")
      l.warn("bar", t)
      l should have('out(Seq(("w", "foo", None), ("w", "bar", Some(t)))))
    }
    "log error" in {
      val l = lm("errorer")
      val t = new Throwable
      l.error("foo")
      l.error("bar", t)
      l should have('out(Seq(("e", "foo", None), ("e", "bar", Some(t)))))
    }
  }


  "The Slf4j logger factory" should {
    "create a logger for a name" in {
      new Slf4jLoggerFactory()("foo.bar") should have('name("foo.bar"))
    }
    "create a root logger" in {
      new Slf4jLoggerFactory()("") should have('name(org.slf4j.Logger.ROOT_LOGGER_NAME))
    }
    "log without errors" in {
      // in lieu of testing that the logging does something (trust the fwk)
      val l = new Slf4jLoggerFactory()("foo.bar")
      l.trace("foo", Some(new Throwable))
      l.debug("foo", Some(new Throwable))
      l.info("foo", Some(new Throwable))
      l.warn("foo", Some(new Throwable))
      l.error("foo", Some(new Throwable))
    }
  }
}
