package com.srednal.snug.config

import com.typesafe.config.Config

/**
 * Conversion from config at path to type X.
 *
 * To create a custom conversion, for example for a case class:
 * {{{
 *   case class Foo(number: Long, pi: Double, interval: Duration, hello: String, yes: Boolean, names: List[String])
 *
 *   implicit object FooConversion extends ConfigConversion[Foo] {
 *     def apply(cfg: Config, path: String) = {
 *        val c: Config = cfg(path)
 *        Foo(c("number"), c("pi"), c("interval"), c("hello"), c("yes"), c("names") )
 *     }
 *   }
 * }}}
 */
trait ConfigConversion[+X] {
  def apply(cfg: Config, path: String): X
}
