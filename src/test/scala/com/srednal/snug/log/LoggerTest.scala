package com.srednal.snug.log

import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.slf4j

object LoggerTest {
  val testObjectLogger = Logger(this)
}

package object xyzzy {
  val testPkgLogger = Logger(this)
}

class LoggerTest extends WordSpec with Matchers with MockFactory {
  import LoggerTest._

  "The Logger object" should {
    "create a logger for a name" in {
      Logger("foo.bar") should have('name("foo.bar"))
    }
    "create a logger for a class" in {
      Logger(this) should have('name("com.srednal.snug.log.LoggerTest"))
    }
    "create a logger for an object" in {
      testObjectLogger should have('name("com.srednal.snug.log.LoggerTest"))
    }
    "create a logger for a package object" in {
      xyzzy.testPkgLogger should have ('name("com.srednal.snug.log.xyzzy"))
    }
    "use slf4j by default" in {
      Logger(this) shouldBe a[Slf4jLogger]
    }
    "create a slf4j root logger from an empty name" in {
      Logger("") should have('name(org.slf4j.Logger.ROOT_LOGGER_NAME))
      Logger(null) should have('name(org.slf4j.Logger.ROOT_LOGGER_NAME))
    }
  }


  "The Slf4j logger factory" should {
    "create a logger for a name" in {
      new Slf4jLoggerFactory()("foo.bar") should have('name("foo.bar"))
    }
    "create a root logger" in {
      new Slf4jLoggerFactory()("") should have('name(org.slf4j.Logger.ROOT_LOGGER_NAME))
    }
    "log via slf4j without errors" in {
      // in lieu of testing that the logging does something (trust the framework to do the right thing)
      val log = new Slf4jLoggerFactory().apply("foo.bar")
      log.trace("foo", Some(new Throwable))
      log.debug("foo", Some(new Throwable))
      log.info("foo", Some(new Throwable))
      log.warn("foo", Some(new Throwable))
      log.error("foo", Some(new Throwable))
    }
  }

  "the Slf4Logger" should {
    "trace" in {
      val log = new Slf4jLogger("test") {
        override val l = mock[slf4j.Logger]
      }

      val t1 = new Throwable("t1")

      (log.l.isTraceEnabled: () => Boolean).expects().returning(true)
      (log.l.trace: (String, java.lang.Throwable) => Unit).expects("foo", t1)

      log.trace("foo", t1)

      (log.l.isTraceEnabled: () => Boolean).expects().returning(true)
      (log.l.trace: (String, java.lang.Throwable) => Unit).expects("bar", null)

      log.trace("bar")

      (log.l.isTraceEnabled: () => Boolean).expects().returning(false)
      log.trace("baz")
    }
    "debug" in {
      val log = new Slf4jLogger("test") {
        override val l = mock[slf4j.Logger]
      }

      val t1 = new Throwable("t1")

      (log.l.isDebugEnabled: () => Boolean).expects().returning(true)
      (log.l.debug: (String, java.lang.Throwable) => Unit).expects("foo", t1)

      log.debug("foo", t1)

      (log.l.isDebugEnabled: () => Boolean).expects().returning(true)
      (log.l.debug: (String, java.lang.Throwable) => Unit).expects("bar", null)

      log.debug("bar")

      (log.l.isDebugEnabled: () => Boolean).expects().returning(false)
      log.debug("baz")
    }
    "info" in {
      val log = new Slf4jLogger("test") {
        override val l = mock[slf4j.Logger]
      }

      val t1 = new Throwable("t1")

      (log.l.isInfoEnabled: () => Boolean).expects().returning(true)
      (log.l.info: (String, java.lang.Throwable) => Unit).expects("foo", t1)

      log.info("foo", t1)

      (log.l.isInfoEnabled: () => Boolean).expects().returning(true)
      (log.l.info: (String, java.lang.Throwable) => Unit).expects("bar", null)

      log.info("bar")

      (log.l.isInfoEnabled: () => Boolean).expects().returning(false)
      log.info("baz")
    }
    "warn" in {
      val log = new Slf4jLogger("test") {
        override val l = mock[slf4j.Logger]
      }

      val t1 = new Throwable("t1")

      (log.l.isWarnEnabled: () => Boolean).expects().returning(true)
      (log.l.warn: (String, java.lang.Throwable) => Unit).expects("foo", t1)

      log.warn("foo", t1)

      (log.l.isWarnEnabled: () => Boolean).expects().returning(true)
      (log.l.warn: (String, java.lang.Throwable) => Unit).expects("bar", null)

      log.warn("bar")

      (log.l.isWarnEnabled: () => Boolean).expects().returning(false)
      log.warn("baz")
    }
    "error" in {
      val log = new Slf4jLogger("test") {
        override val l = mock[slf4j.Logger]
      }

      val t1 = new Throwable("t1")

      (log.l.isErrorEnabled: () => Boolean).expects().returning(true)
      (log.l.error: (String, java.lang.Throwable) => Unit).expects("foo", t1)

      log.error("foo", t1)

      (log.l.isErrorEnabled: () => Boolean).expects().returning(true)
      (log.l.error: (String, java.lang.Throwable) => Unit).expects("bar", null)

      log.error("bar")

      (log.l.isErrorEnabled: () => Boolean).expects().returning(false)
      log.error("baz")
    }
  }

  "a Logger" should {
    "allow messages to be lazy" in {
      var evaluated: String = ""

      Logger.Silent.trace({evaluated = "trace"; "omg"})
      Logger.Silent.trace({evaluated = "trace1"; "omg"}, new Throwable)
      Logger.Silent.trace({evaluated = "trace2"; "omg"}, Some(new Throwable))
      Logger.Silent.trace({evaluated = "trace3"; "omg"}, None)
      Logger.Silent.trace({evaluated = "trace4"; this})

      Logger.Silent.debug({evaluated = "debug"; "omg"})
      Logger.Silent.debug({evaluated = "debug1"; "omg"}, new Throwable)
      Logger.Silent.debug({evaluated = "debug2"; "omg"}, Some(new Throwable))
      Logger.Silent.debug({evaluated = "debug3"; "omg"}, None)
      Logger.Silent.debug({evaluated = "debug4"; this})

      Logger.Silent.info({evaluated = "info"; "omg"})
      Logger.Silent.info({evaluated = "info1"; "omg"}, new Throwable)
      Logger.Silent.info({evaluated = "info2"; "omg"}, Some(new Throwable))
      Logger.Silent.info({evaluated = "info3"; "omg"}, None)

      Logger.Silent.warn({evaluated = "warn"; "omg"})
      Logger.Silent.warn({evaluated = "warn1"; "omg"}, new Throwable)
      Logger.Silent.warn({evaluated = "warn2"; "omg"}, Some(new Throwable))
      Logger.Silent.warn({evaluated = "warn3"; "omg"}, None)

      Logger.Silent.error({evaluated = "error"; "omg"})
      Logger.Silent.error({evaluated = "error1"; "omg"}, new Throwable)
      Logger.Silent.error({evaluated = "error2"; "omg"}, Some(new Throwable))
      Logger.Silent.error({evaluated = "error3"; "omg"}, None)

      evaluated shouldBe ""
    }

    "log trace" in {
      // need to mock the function rather than the Logger class because I can't figure out how to handle the
      // expect/verify of the m: => String arg in the Logger methods via mock[Logger]
      val mf = mockFunction[String, Option[Throwable], Unit]
      val l = new Logger {
        override def trace(m: => String, t: Option[Throwable]): Unit = mf(m, t)
        override def debug(m: => String, t: Option[Throwable]): Unit = ???
        override def info(m: => String, t: Option[Throwable]): Unit = ???
        override def warn(m: => String, t: Option[Throwable]): Unit = ???
        override def error(m: => String, t: Option[Throwable]): Unit = ???
      }

      val t = new Throwable

      inSequence {
        mf.expects("foo", None)
        mf.expects("bar", Some(t))
        mf.expects(this.toString(), None)
        mf.expects("this is the stuff", None)
      }

      l.trace("foo")
      l.trace("bar", t)
      l.trace(this)
      l.tracing {
        "this is the stuff"
      } shouldBe "this is the stuff"
    }

    "log debug" in {
      // with a stub (vs mock) you verify after-the-fact rather than expect ahead of time
      val mf = stubFunction[String, Option[Throwable], Unit]
      val l = new Logger {
        override def trace(m: => String, t: Option[Throwable]): Unit = ???
        override def debug(m: => String, t: Option[Throwable]): Unit = mf(m, t)
        override def info(m: => String, t: Option[Throwable]): Unit = ???
        override def warn(m: => String, t: Option[Throwable]): Unit = ???
        override def error(m: => String, t: Option[Throwable]): Unit = ???
      }

      val t = new Throwable

      l.debug("foo")
      l.debug("bar", t)
      l.debug(this)
      l.debuging {
        "this is the stuff"
      } shouldBe "this is the stuff"

      inSequence {
        mf.verify("foo", None)
        mf.verify("bar", Some(t))
        mf.verify(this.toString(), None)
        mf.verify("this is the stuff", None)
      }
    }

    "log info" in {
      val mf = stubFunction[String, Option[Throwable], Unit]
      val l = new Logger {
        override def trace(m: => String, t: Option[Throwable]): Unit = ???
        override def debug(m: => String, t: Option[Throwable]): Unit = ???
        override def info(m: => String, t: Option[Throwable]): Unit = mf(m, t)
        override def warn(m: => String, t: Option[Throwable]): Unit = ???
        override def error(m: => String, t: Option[Throwable]): Unit = ???
      }

      val t = new Throwable

      l.info("foo")
      l.info("bar", t)

      inSequence {
        mf.verify("foo", None)
        mf.verify("bar", Some(t))
      }
    }

    "log warn" in {
      val mf = stubFunction[String, Option[Throwable], Unit]
      val l = new Logger {
        override def trace(m: => String, t: Option[Throwable]): Unit = ???
        override def debug(m: => String, t: Option[Throwable]): Unit = ???
        override def info(m: => String, t: Option[Throwable]): Unit = ???
        override def warn(m: => String, t: Option[Throwable]): Unit = mf(m, t)
        override def error(m: => String, t: Option[Throwable]): Unit = ???
      }

      val t = new Throwable

      l.warn("foo")
      l.warn("bar", t)

      inSequence {
        mf.verify("foo", None)
        mf.verify("bar", Some(t))
      }
    }

    "log error" in {
      val mf = stubFunction[String, Option[Throwable], Unit]
      val l = new Logger {
        override def trace(m: => String, t: Option[Throwable]): Unit = ???
        override def debug(m: => String, t: Option[Throwable]): Unit = ???
        override def info(m: => String, t: Option[Throwable]): Unit = ???
        override def warn(m: => String, t: Option[Throwable]): Unit = ???
        override def error(m: => String, t: Option[Throwable]): Unit = mf(m, t)
      }

      val t = new Throwable

      l.error("foo")
      l.error("bar", t)

      inSequence {
        mf.verify("foo", None)
        mf.verify("bar", Some(t))
      }
    }
  }
}
