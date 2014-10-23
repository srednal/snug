package com.srednal.snug.log

import com.srednal.snug.ServiceLoader


trait Logger {
  def trace(m: => String, t: Option[Throwable] = None): Unit
  def trace(m: => String, t: Throwable): Unit = trace(m, Some(t))
  def trace(x: => Any): Unit = trace(x.toString, None)

  def tracing[T](f: => T): T = {
    val m = f
    trace(m)
    m
  }

  def debug(m: => String, t: Option[Throwable] = None): Unit
  def debug(m: => String, t: Throwable): Unit = debug(m, Some(t))
  def debug(x: => Any): Unit = debug(x.toString, None)

  def debuging[T](f: => T): T = {
    val m = f
    debug(m)
    m
  }

  def info(m: => String, t: Option[Throwable] = None): Unit
  def info(m: => String, t: Throwable): Unit = info(m, Some(t))

  def warn(m: => String, t: Option[Throwable] = None): Unit
  def warn(m: => String, t: Throwable): Unit = warn(m, Some(t))

  def error(m: => String, t: Option[Throwable] = None): Unit
  def error(m: => String, t: Throwable): Unit = error(m, Some(t))
}

/* This is split out so tests can direct to a test Logger instance */
private[log] trait LoggerMaker {
  val factory: String => Logger = ServiceLoader[LoggerFactory] getOrElse { _: String => Logger.Silent}

  def apply(thiz: Any): Logger = apply(thiz.getClass.getName)

  def apply(name: String): Logger = name match {
    case null | "" => factory("") // root logger
    case objName if objName.last == '$' => apply(objName.init) // allows object Foo { val logger = Logger(this) }
    case n => factory(n)
  }
}


object Logger extends LoggerMaker {

  object Silent extends Logger {
    def trace(m: => String, t: Option[Throwable]) = ()
    def debug(m: => String, t: Option[Throwable]) = ()
    def info(m: => String, t: Option[Throwable]) = ()
    def warn(m: => String, t: Option[Throwable]) = ()
    def error(m: => String, t: Option[Throwable]) = ()
  }
}


trait LoggerFactory extends (String => Logger) {
  def apply(n: String): Logger
}
