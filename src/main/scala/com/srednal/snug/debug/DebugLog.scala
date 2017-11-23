package com.srednal.snug.debug

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import com.typesafe.scalalogging.Logger

/** Intended to be mixed-in with, for example, LazyLogging or StrictLogging,
  * however the only restriction is that logger is defined.
  */
trait DebugLog {

  def logger: Logger

  // debuglog(foo) => logger.debug(s"foo = $foo")
  // debuglog(foo, bar) => logger.debug(s"foo = $foo"); logger.debug(s"bar = $bar")
  // debuglog("foo", bar) => logger.debug("foo"); logger.debug(s"bar = $bar")
  def debuglog(params: Any*): Unit = macro DebugMacroBundle.debug
  def tracelog(params: Any*): Unit = macro DebugMacroBundle.trace

}

class DebugMacroBundle(val c: blackbox.Context) {

  // scalastyle:ignore import.grouping
  import c.universe._

  // call logger.debug once for each param
  def debug(params: c.Expr[Any]*): Block =
    Block(params.toList map paramDebugString map { s => q"${c.prefix}.logger.debug($s)" }, Literal(Constant(())))
  def trace(params: c.Expr[Any]*): Block =
    Block(params.toList map paramDebugString map { s => q"${c.prefix}.logger.trace($s)" }, Literal(Constant(())))


  // a constant literal results in simply the value
  // otherwise fetch the param name and use that in the log as "name = value"
  def paramDebugString(param: c.Expr[Any]): Tree = param.tree match {
    case c.universe.Literal(c.universe.Constant(_)) => q"$param.toString"
    case t =>
      val name = c.Expr[String](Literal(Constant(show(t))))
      reify {name.splice + " = " + param.splice}.tree
  }
}

