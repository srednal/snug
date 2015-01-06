package com.srednal.snug.log

import org.slf4j

class Slf4jLoggerFactory extends LoggerFactory {
  // initialize slf4j early(ish)
  slf4j.LoggerFactory.getILoggerFactory

  def apply(name: String) = new Slf4jLogger(name match {
    case "" | null => slf4j.Logger.ROOT_LOGGER_NAME // scalastyle:ignore null
    case n => n
  })
}

class Slf4jLogger(val name: String) extends Logger {
  val l = slf4j.LoggerFactory.getLogger(name)
  override def trace(m: => String, t: Option[Throwable]) = if (l.isTraceEnabled) l.trace(m, t.orNull)
  override def debug(m: => String, t: Option[Throwable]) = if (l.isDebugEnabled) l.debug(m, t.orNull)
  override def info(m: => String, t: Option[Throwable]) = if (l.isInfoEnabled) l.info(m, t.orNull)
  override def warn(m: => String, t: Option[Throwable]) = if (l.isWarnEnabled) l.warn(m, t.orNull)
  override def error(m: => String, t: Option[Throwable]) = if (l.isErrorEnabled) l.error(m, t.orNull)
}
