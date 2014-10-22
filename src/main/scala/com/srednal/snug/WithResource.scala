package com.srednal.snug

import java.io.Closeable
import scala.io.Source
import scala.util.control.NonFatal

object WithResource {

  def withSource[S <: Source, T](s: S)(f: S => T): T = withCloser[T](s.close())(f(s))

  def withResource[R <: Closeable, T](r: R)(f: R => T): T = withCloser[T](r.close())(f(r))

  def withCloser[T](closer: => Unit)(f: => T): T =
    try f
    finally
      try closer
      catch {
        case NonFatal(_) =>
      }
}
