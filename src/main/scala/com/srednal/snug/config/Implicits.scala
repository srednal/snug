package com.srednal.snug.config

import akka.util.Timeout
import com.typesafe.config.Config
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.language.implicitConversions

trait Implicits extends ExtraImplicits {

  implicit object SubPathConversion extends ConfigConversion[Config] {
    def apply(cfg: Config, path: String) = cfg.getConfig(path)
  }


  implicit object StringConversion extends ConfigConversion[String] {
    def apply(cfg: Config, path: String) = cfg.getValue(path).unwrapped().toString
  }

  implicit object DurationConversion extends ConfigConversion[FiniteDuration] {
    def apply(cfg: Config, path: String) = cfg.getDuration(path, NANOSECONDS).nanos
  }

  implicit object TimeoutConversion extends ConfigConversion[Timeout] {
    def apply(cfg: Config, path: String) = DurationConversion(cfg, path)
  }

  case class OptionConversion[X](inner: ConfigConversion[X]) extends ConfigConversion[Option[X]] {
    def apply(cfg: Config, path: String) = if (cfg.hasPath(path)) Some(cfg[X](path)(inner)) else None
  }

  implicit def asOption[X](implicit inner: ConfigConversion[X]): ConfigConversion[Option[X]] = OptionConversion[X](inner)


  case class ListConversion[+X](inner: ConfigConversion[X]) extends ConfigConversion[List[X]] {
    def apply(cfg: Config, path: String) = cfg.getList(path).asScala.toList map (_.atKey("X")) map (new RichConfig(_)) map (_("X")(inner))
  }

  implicit def asList[X](implicit inner: ConfigConversion[X]): ConfigConversion[List[X]] = ListConversion[X](inner)

  implicit object NumberConversion extends ConfigConversion[Number] {
    def apply(cfg: Config, path: String) = cfg.getNumber(path)
  }

  implicit object DoubleConversion extends ConfigConversion[Double] {
    def apply(cfg: Config, path: String) = NumberConversion(cfg, path).doubleValue()
  }

  implicit object FloatConversion extends ConfigConversion[Float] {
    def apply(cfg: Config, path: String) = NumberConversion(cfg, path).floatValue()
  }

  implicit object LongConversion extends ConfigConversion[Long] {
    def apply(cfg: Config, path: String) = NumberConversion(cfg, path).longValue()
  }

  implicit object IntConversion extends ConfigConversion[Int] {
    def apply(cfg: Config, path: String) = NumberConversion(cfg, path).intValue()
  }


  implicit object BooleanConversion extends ConfigConversion[Boolean] {
    def apply(cfg: Config, path: String) = cfg.getBoolean(path)
  }

}

trait ExtraImplicits {

  // allow implicit for ConfigConversion[Traversable | Iterable | Seq] to use asList above (unambiguously)
  // ConfigConversion[Set] will drop in here
  case class SetConversion[X](inner: ConfigConversion[X]) extends ConfigConversion[Set[X]] {
    def apply(cfg: Config, path: String) = ListConversion(inner)(cfg, path).toSet
  }

  implicit def asSet[X](implicit inner: ConfigConversion[X]): ConfigConversion[Set[X]] = SetConversion[X](inner)
}
