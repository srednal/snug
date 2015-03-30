package com.srednal.snug.config

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try
import java.net.{InetAddress, InetSocketAddress, URI, URL}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.config.ConfigException.WrongType

import scala.language.implicitConversions

trait Implicits extends ContainerImplicits {

  private def str(cfg: Config, path: String) = cfg.getValue(path).unwrapped().toString

  implicit object StringConversion extends ConfigConversion[String] {
    override def get(cfg: Config, path: String) = str(cfg, path)
  }

  private def int(cfg: Config, path: String) = BigInt(str(cfg, path))

  implicit object BigIntConversion extends ConfigConversion[BigInt] {
    override def get(cfg: Config, path: String) = int(cfg, path)
  }

  implicit object LongConversion extends ConfigConversion[Long] {
    override def get(cfg: Config, path: String) = int(cfg, path).longValue()
  }

  implicit object IntConversion extends ConfigConversion[Int] {
    override def get(cfg: Config, path: String) = int(cfg, path).intValue()
  }

  private def dec(cfg: Config, path: String) = BigDecimal(str(cfg, path))

  implicit object BigDecimalConversion extends ConfigConversion[BigDecimal] {
    override def get(cfg: Config, path: String) = dec(cfg, path)
  }

  implicit object DoubleConversion extends ConfigConversion[Double] {
    override def get(cfg: Config, path: String) = dec(cfg, path).doubleValue()
  }

  implicit object FloatConversion extends ConfigConversion[Float] {
    override def get(cfg: Config, path: String) = dec(cfg, path).floatValue()
  }

  implicit object BooleanConversion extends ConfigConversion[Boolean] {
    override def get(cfg: Config, path: String) = cfg.getBoolean(path)
  }

  implicit object DurationConversion extends ConfigConversion[FiniteDuration] {
    override def get(cfg: Config, path: String) = cfg.getDuration(path, NANOSECONDS).nanos
  }

  implicit object TimeoutConversion extends ConfigConversion[Timeout] {
    override def get(cfg: Config, path: String) = DurationConversion.get(cfg, path)
  }


  implicit object URIConversion extends ConfigConversion[URI] {
    override def get(cfg: Config, path: String) = new URI(str(cfg, path))
  }

  implicit object URLConversion extends ConfigConversion[URL] {
    override def get(cfg: Config, path: String) = URIConversion.get(cfg, path).toURL
  }

  implicit object InetAddressConversion extends ConfigConversion[InetAddress] {
    override def get(cfg: Config, path: String) = InetAddress.getByName(str(cfg, path))
  }

  implicit object InetSocketAddressConversion extends ConfigConversion[InetSocketAddress] {
    private val HostPort = """^(?:(.+):)?([0-9]+)$""".r
    override def get(cfg: Config, path: String) = str(cfg, path) match {
      case HostPort(host: String, port) => new InetSocketAddress(host, port.toInt)
      case HostPort(_, port) => new InetSocketAddress(port.toInt)
      case v => throw new WrongType(cfg.getValue(path).origin(), s"$path is '$v' rather than a <host:port> or <port>")
    }
  }

  implicit object SubPathConversion extends ConfigConversion[Config] {
    override def get(cfg: Config, path: String) = cfg.getConfig(path)
  }

}


trait ContainerImplicits extends LowerPriorityContainerImplicits {

  private class OptionConversion[+X: ConfigConversion] extends ConfigConversion[Option[X]] {
    override def get(cfg: Config, path: String) = if (cfg.hasPath(path)) Some(cfg(path)) else None
  }

  implicit def configConvertForOption[X: ConfigConversion]: ConfigConversion[Option[X]] = new OptionConversion[X]

  private class TryConversion[+X: ConfigConversion] extends ConfigConversion[Try[X]] {
    override def get(cfg: Config, path: String) = Try(cfg(path))
  }

  implicit def configConvertForTry[X: ConfigConversion]: ConfigConversion[Try[X]] = new TryConversion[X]

  private class ListConversion[+X: ConfigConversion] extends ConfigConversion[List[X]] {
    override def get(cfg: Config, path: String) =
      cfg.getList(path).asScala.toList map (_.atKey("X")[X]("X"))
  }

  implicit def configConvertForList[X: ConfigConversion]: ConfigConversion[List[X]] = new ListConversion[X]

}

trait LowerPriorityContainerImplicits {

  // allow implicit for ConfigConversion[Traversable | Iterable | Seq] to use configConvertForList above (unambiguously)
  // ConfigConversion[Set] will drop in here
  private class SetConversion[X: ConfigConversion] extends ConfigConversion[Set[X]] {
    override def get(cfg: Config, path: String) = configConvertForList[X].get(cfg, path).toSet
  }

  implicit def configConvertForSet[X: ConfigConversion]: ConfigConversion[Set[X]] = new SetConversion[X]

}
