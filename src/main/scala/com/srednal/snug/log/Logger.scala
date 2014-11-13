package com.srednal.snug.log

import com.srednal.snug.ServiceLoader


trait Logger {
  def trace(m: => String, t: Option[Throwable]): Unit
  def trace(m: => String, t: Throwable): Unit = trace(m, Some(t))
//  def trace(m: => String): Unit = trace(m, None)
  def trace(x: => Any): Unit = trace(x.toString, None)

  def tracing[T](f: => T): T = {
    val m = f
    trace(m)
    m
  }

  def debug(m: => String, t: Option[Throwable] = None): Unit
  def debug(m: => String, t: Throwable): Unit = debug(m, Some(t))
//  def debug(m: => String): Unit = debug(m, None)
  def debug(x: => Any): Unit = debug(x.toString, None)

  def debuging[T](f: => T): T = {
    val m = f
    debug(m)
    m
  }

  def info(m: => String, t: Option[Throwable]): Unit
  def info(m: => String, t: Throwable): Unit = info(m, Some(t))
  def info(m: => String): Unit = info(m, None)

  def warn(m: => String, t: Option[Throwable]): Unit
  def warn(m: => String, t: Throwable): Unit = warn(m, Some(t))
  def warn(m: => String): Unit = warn(m, None)

  def error(m: => String, t: Option[Throwable]): Unit
  def error(m: => String, t: Throwable): Unit = error(m, Some(t))
  def error(m: => String): Unit = error(m, None)
}


object Logger {
  val factory: String => Logger = ServiceLoader[LoggerFactory] getOrElse { _: String => Silent}

  def apply(thiz: Any): Logger = apply(thiz.getClass.getName)

  def apply(name: String): Logger = name match {
    case null | "" => factory("") // root logger
    case objName if objName.last == '$' => apply(objName.init) // allows object Foo { val logger = Logger(this) }
    case n => factory(n)
  }

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
