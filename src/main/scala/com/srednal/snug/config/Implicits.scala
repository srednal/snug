package com.srednal.snug.config

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try
import java.net.{InetAddress, InetSocketAddress, URI, URL}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.config.ConfigException.WrongType

import scala.language.implicitConversions

trait Implicits extends ExtraImplicits {

  implicit object SubPathConversion extends ConfigConversion[Config] {
    override def get(cfg: Config, path: String) = cfg.getConfig(path)
  }

  implicit object StringConversion extends ConfigConversion[String] {
    override def get(cfg: Config, path: String) = cfg.getValue(path).unwrapped().toString
  }

  implicit object DurationConversion extends ConfigConversion[FiniteDuration] {
    override def get(cfg: Config, path: String) = cfg.getDuration(path, NANOSECONDS).nanos
  }

  implicit object TimeoutConversion extends ConfigConversion[Timeout] {
    override def get(cfg: Config, path: String) = DurationConversion.get(cfg, path)
  }

  case class OptionConversion[+X](inner: ConfigConversion[X]) extends ConfigConversion[Option[X]] {
    override def get(cfg: Config, path: String) = if (cfg.hasPath(path)) Some(cfg[X](path)(inner)) else None
  }

  implicit def optionConfigCvt[X](implicit inner: ConfigConversion[X]): OptionConversion[X] = OptionConversion[X](inner)

  case class TryConversion[+X](inner: ConfigConversion[X]) extends ConfigConversion[Try[X]] {
    override def get(cfg: Config, path: String) = Try(cfg[X](path)(inner))
  }

  implicit def tryConfigCvt[X](implicit inner: ConfigConversion[X]): TryConversion[X] = TryConversion[X](inner)

  case class ListConversion[+X](inner: ConfigConversion[X]) extends ConfigConversion[List[X]] {
    override def get(cfg: Config, path: String) = cfg.getList(path).asScala.toList map (_.atKey("X")) map (new RichConfig(_)) map (_("X")(inner))
  }

  implicit def listConfigCvt[X](implicit inner: ConfigConversion[X]): ListConversion[X] = ListConversion[X](inner)

  implicit object BigDecimalConversion extends ConfigConversion[BigDecimal] {
    override def get(cfg: Config, path: String) = BigDecimal(StringConversion.get(cfg, path))
  }

  implicit object DoubleConversion extends ConfigConversion[Double] {
    override def get(cfg: Config, path: String) = BigDecimalConversion.get(cfg, path).doubleValue()
  }

  implicit object FloatConversion extends ConfigConversion[Float] {
    override def get(cfg: Config, path: String) = BigDecimalConversion.get(cfg, path).floatValue()
  }

  implicit object BigIntConversion extends ConfigConversion[BigInt] {
    override def get(cfg: Config, path: String) = BigInt(StringConversion.get(cfg, path))
  }

  implicit object LongConversion extends ConfigConversion[Long] {
    override def get(cfg: Config, path: String) = BigIntConversion.get(cfg, path).longValue()
  }

  implicit object IntConversion extends ConfigConversion[Int] {
    override def get(cfg: Config, path: String) = BigIntConversion.get(cfg, path).intValue()
  }

  implicit object BooleanConversion extends ConfigConversion[Boolean] {
    override def get(cfg: Config, path: String) = cfg.getBoolean(path)
  }

  implicit object URIConversion extends ConfigConversion[URI] {
    override def get(cfg: Config, path: String) = new URI(StringConversion.get(cfg, path))
  }

  implicit object URLConversion extends ConfigConversion[URL] {
    override def get(cfg: Config, path: String) = URIConversion.get(cfg, path).toURL
  }

  implicit object InetAddressConversion extends ConfigConversion[InetAddress] {
    override def get(cfg: Config, path: String) = InetAddress.getByName(StringConversion.get(cfg, path))
  }


  implicit object InetSocketAddressConversion extends ConfigConversion[InetSocketAddress] {
    private val HostPort = """^(?:(.+):)?([0-9]+)$""".r
    override def get(cfg: Config, path: String) = StringConversion.get(cfg, path) match {
      case HostPort(host: String, port) => new InetSocketAddress(host, port.toInt)
      case HostPort(_, port) => new InetSocketAddress(port.toInt)
      case v => throw new WrongType(cfg.getValue(path).origin(), s"$path is '$v' rather than a <host:port> or <port>")
    }
  }

}

trait ExtraImplicits {

  // allow implicit for ConfigConversion[Traversable | Iterable | Seq] to use asList above (unambiguously)
  // ConfigConversion[Set] will drop in here
  case class SetConversion[X](inner: ConfigConversion[X]) extends ConfigConversion[Set[X]] {
    override def get(cfg: Config, path: String) = ListConversion(inner).get(cfg, path).toSet
  }

  implicit def setConfigCvt[X: ConfigConversion]: ConfigConversion[Set[X]] = SetConversion[X](ConfigConversion[X])
}
