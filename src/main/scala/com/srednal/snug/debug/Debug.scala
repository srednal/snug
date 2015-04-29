package com.srednal.snug.debug

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Intended to be mixed-in with, for example, LazyLogging or StrictLogging,
  * however the only restriction is that logger.debug(String) can be called somehow.
  */
trait Debug {
  // debug(foo) => logger.debug(s"foo = $foo")
  // debug(foo, bar) => logger.debug(s"foo = $foo, bar = $bar")
  // debug("foo", bar) => logger.debug(s"foo, bar = $bar")
  // debug(foo) => logger.debug(s"foo = $foo")
  def debug(params: Any*): Unit = macro DebugMacroBundle.debug

}

class DebugMacroBundle(val c: blackbox.Context) {

  // scalastyle:ignore import.grouping
  import c.universe._

  def debug(params: c.Expr[Any]*): c.Expr[Unit] = {
    val out = join(params.toList map paramDebugString).tree
    c.Expr[Unit]( q"""logger.debug($out)""")
  }

  def paramDebugString(param: c.Expr[Any]): c.Expr[String] = param.tree match {
    case c.universe.Literal(c.universe.Constant(_)) =>
      reify {
        param.splice.toString
      }
    case t =>
      val name = c.Expr[String](Literal(Constant(show(t))))
      reify {
        name.splice + " = " + param.splice
      }
  }

  def join(ex: List[c.Expr[String]]): c.Expr[String] = ex match {
    case Nil => reify("")
    case h :: Nil => reify {h.splice}
    case h :: t => reify {h.splice + ", " + join(t).splice}
  }
}

