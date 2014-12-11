package com.srednal.snug.config

import java.net.{InetSocketAddress, InetAddress, URL, URI}

import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.config.ConfigException.WrongType
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.util.Try

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

  case class OptionConversion[+X](inner: ConfigConversion[X]) extends ConfigConversion[Option[X]] {
    def apply(cfg: Config, path: String) = if (cfg.hasPath(path)) Some(cfg[X](path)(inner)) else None
  }

  implicit def optionConfigCvt[X](implicit inner: ConfigConversion[X]): OptionConversion[X] = OptionConversion[X](inner)

  case class TryConversion[+X](inner: ConfigConversion[X]) extends ConfigConversion[Try[X]] {
    def apply(cfg: Config, path: String) = Try(cfg[X](path)(inner))
  }

  implicit def tryConfigCvt[X](implicit inner: ConfigConversion[X]): TryConversion[X] = TryConversion[X](inner)

  case class ListConversion[+X](inner: ConfigConversion[X]) extends ConfigConversion[List[X]] {
    def apply(cfg: Config, path: String) = cfg.getList(path).asScala.toList map (_.atKey("X")) map (new RichConfig(_)) map (_("X")(inner))
  }

  implicit def listConfigCvt[X](implicit inner: ConfigConversion[X]): ListConversion[X] = ListConversion[X](inner)

  implicit object BigDecimalConversion extends ConfigConversion[BigDecimal] {
    def apply(cfg: Config, path: String) = BigDecimal(StringConversion(cfg, path))
  }

  implicit object DoubleConversion extends ConfigConversion[Double] {
    def apply(cfg: Config, path: String) = BigDecimalConversion(cfg, path).doubleValue()
  }

  implicit object FloatConversion extends ConfigConversion[Float] {
    def apply(cfg: Config, path: String) = BigDecimalConversion(cfg, path).floatValue()
  }

  implicit object BigIntConversion extends ConfigConversion[BigInt] {
    def apply(cfg: Config, path: String) = BigInt(StringConversion(cfg, path))
  }

  implicit object LongConversion extends ConfigConversion[Long] {
    def apply(cfg: Config, path: String) = BigIntConversion(cfg, path).longValue()
  }

  implicit object IntConversion extends ConfigConversion[Int] {
    def apply(cfg: Config, path: String) = BigIntConversion(cfg, path).intValue()
  }

  implicit object BooleanConversion extends ConfigConversion[Boolean] {
    def apply(cfg: Config, path: String) = cfg.getBoolean(path)
  }

  implicit object URIConversion extends ConfigConversion[URI] {
    def apply(cfg: Config, path: String) = new URI(StringConversion(cfg, path))
  }

  implicit object URLConversion extends ConfigConversion[URL] {
    def apply(cfg: Config, path: String) = URIConversion(cfg, path).toURL
  }

  implicit object InetAddressConversion extends ConfigConversion[InetAddress] {
    def apply(cfg: Config, path: String) = InetAddress.getByName(StringConversion(cfg, path))
  }


  implicit object InetSocketAddressConversion extends ConfigConversion[InetSocketAddress] {
    private val HostPort = """^(?:(.+):)?([0-9]+)$""".r
    def apply(cfg: Config, path: String) = StringConversion(cfg, path) match {
      case HostPort(null, port) => new InetSocketAddress(port.toInt)
      case HostPort(host, port) => new InetSocketAddress(host, port.toInt)
      case v => throw new WrongType(cfg.getValue(path).origin(), s"$path is '$v' rather than a <host:port> or <port>")
    }
  }

}

trait ExtraImplicits {

  // allow implicit for ConfigConversion[Traversable | Iterable | Seq] to use asList above (unambiguously)
  // ConfigConversion[Set] will drop in here
  case class SetConversion[X](inner: ConfigConversion[X]) extends ConfigConversion[Set[X]] {
    def apply(cfg: Config, path: String) = ListConversion(inner)(cfg, path).toSet
  }

  implicit def setConfigCvt[X](implicit inner: ConfigConversion[X]): ConfigConversion[Set[X]] = SetConversion[X](inner)
}
