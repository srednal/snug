package com.srednal.snug

import com.typesafe.config.{Config, ConfigFactory}
import scala.concurrent.duration._
import scala.collection.JavaConverters._

package object config {

  val config: RichConfig = ConfigFactory.load()

  /** cstr"foo.bar" => config.string("foo.bar") */
  implicit class ConfigStringContext(val sc: StringContext) extends AnyVal {
    private def path(args: Seq[Any]) = sc.standardInterpolator(StringContext.treatEscapes, args)
    def cstr(args: Any*): String = config.string(path(args))
    def cint(args: Any*): Int = config.int(path(args))
    def clong(args: Any*): Long = config.long(path(args))
    def cdbl(args: Any*): Double = config.double(path(args))
    def cbool(args: Any*): Boolean = config.boolean(path(args))
    def cdur(args: Any*): FiniteDuration =  config.duration(path(args))
  }

  implicit class RichConfig(val cfg: Config) {
    def apply(path: String): RichConfig = cfg.getConfig(path)
    def string(path: String): String = cfg.getString(path)
    def int(path: String): Int = cfg.getInt(path)
    def long(path: String): Long = cfg.getLong(path)
    def double(path: String): Double = cfg.getDouble(path)
    def boolean(path: String): Boolean = cfg.getBoolean(path)
    def duration(path: String): FiniteDuration = cfg.getDuration(path, NANOSECONDS).nanos
    def seqString(path: String): Seq[String] = cfg.getStringList(path).asScala
  }
}
