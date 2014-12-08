package com.srednal.snug

import com.typesafe.config._

package object config extends Implicits {

  /** Global/default configuration */
  val config = ConfigFactory.load()

  /**
   * Pimp a Config with some helpful conversions
   */
  implicit class RichConfig(val cfg: Config) {

    /**
     * Fetch the value at path converted/interpreted as a T.
     *
     * As in:   config[String]("foo")
     * or:      val foo: String = config("foo")
     *
     * Type T can be any of:
     * - String (for any config type).
     * - Int, Long, Double, etc (for config numbers).
     * - Boolean (for config booleans).
     * - Duration, FiniteDuration, or Timeout (when config can parse it as a duration).
     * - Traversable[T] or Set[T] (for config lists).
     * - Option[T] (None where no config at the path).
     * - Another Config object.
     * - Anything else (i.e. a case class) where a (custom) ConfigConversion[T] is provided (or implicitly available in scope).
     */
    def apply[T](path: String)(implicit cvt: ConfigConversion[T]): T = cvt(cfg, path)
  }
}
