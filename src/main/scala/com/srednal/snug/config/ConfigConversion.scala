package com.srednal.snug.config

import scala.annotation.implicitNotFound
import com.typesafe.config.Config

/**
 * Type Class: Conversion from config at path to type X.
 *
 * To create a custom conversion, for example for a case class:
 * {{{
 *   case class Foo(number: Long, pi: Double, interval: Duration, hello: String, yes: Boolean, names: List[String])
 *
 *   implicit object FooConversion extends ConfigConversion[Foo] {
 *     override def get(cfg: Config, path: String) = {
 *        val c: Config = cfg(path)
 *        Foo(c("number"), c("pi"), c("interval"), c("hello"), c("yes"), c("names") )
 *     }
 *   }
 * }}}
 */
@implicitNotFound("No member of type class ConfigConversion found for type ${X}")
trait ConfigConversion[+X] {
  def get(cfg: Config, path: String): X
}

object ConfigConversion {
  def apply[X: ConfigConversion]: ConfigConversion[X] = implicitly[ConfigConversion[X]]
}
