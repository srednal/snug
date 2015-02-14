package com.srednal.snug

import scala.util.control.NonFatal
import com.typesafe.config._
import com.typesafe.scalalogging.LazyLogging

package object config extends Implicits with LazyLogging {

  /** Global/default configuration */
  val config = ConfigFactory.load()

  /**
   * Pimp a Config with some helpful conversions
   */
  implicit class RichConfig(val cfg: Config) {

    /**
     * Fetch the value at path converted/interpreted as a T.
     *
     * As in:
     * {{{
     *   import com.srednal.snug.config._
     *
     *   val foo = config[String]("foo")
     *   val bar: Int = config("bar")
     *   val baz = config[Option[List[Boolean]]("baz")
     * }}}
     *
     * Type T can be any of:
     * - String (for any config type - note you probably won't like what this does with objects and lists).
     * - Int, Long, Double, Float, BigInt, BigDecimal (where can be parsed as such).
     * - Boolean (for config booleans).
     * - Duration, FiniteDuration, or Timeout (when config can parse it as a HOCON duration).
     * - URI or URL.
     * - InetAddress.
     * - InetSocketAddress (from a string of the form host:port or an integer port).
     * - Traversable[T] or Set[T] (for config lists). Note: must be a list in the config (does not "promote" non-lists to one-element lists).
     * - Option[T] - None where no config at the path (and does not log no path).
     * - Try[T] - Failure if any error (no path, conversion, etc - and does not log).
     * - A Config object (containing the Config under that path).
     * - Anything else (i.e. a case class) where a (custom) ConfigConversion[T] is provided (or implicitly available in scope).
     *
     * Except as noted above, errors (missing path, type conversion error, etc) are logged.
     */
    def apply[T](path: String)(implicit cvt: ConfigConversion[T]): T =
      try cvt(cfg, path)
      catch {
        case NonFatal(e) =>
          val msg =
            if (cfg hasPath path) s"Error parsing config at $path from ${cfg.getValue(path).origin().description()}"
            else s"Config not defined at $path"
          logger.error(msg, e)
          throw e
      }
  }
}
