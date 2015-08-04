package com.srednal.snug.config

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.util.Try
import java.net.{InetAddress, InetSocketAddress, URI, URL}
import akka.util.Timeout
import com.srednal.snug.{Path, ByteSize}
import com.typesafe.config.Config
import com.typesafe.config.ConfigException.WrongType

trait Implicits extends ContainerImplicits {

  private implicit class Chain[A, B](a: (Config, String) => A) {
    // scalastyle:ignore method.name
    def |>(b: A => B) = (cfg: Config, path: String) => b(a(cfg, path))
  }

  implicit object StringConversion extends ConfigConversionAux[String](_.getValue(_).unwrapped().toString)

  implicit object BigIntConversion extends ConfigConversionAux[BigInt](StringConversion.g |> BigInt.apply)

  implicit object LongConversion extends ConfigConversionAux[Long](BigIntConversion.g |> (_.longValue()))

  implicit object IntConversion extends ConfigConversionAux[Int](BigIntConversion.g |> (_.intValue()))

  implicit object BigDecimalConversion extends ConfigConversionAux[BigDecimal](StringConversion.g |> BigDecimal.apply)

  implicit object DoubleConversion extends ConfigConversionAux[Double](BigDecimalConversion.g |> (_.doubleValue()))

  implicit object FloatConversion extends ConfigConversionAux[Float](BigDecimalConversion.g |> (_.floatValue()))

  implicit object BooleanConversion extends ConfigConversionAux[Boolean](_ getBoolean _)

  implicit object DurationConversion extends ConfigConversionAux[FiniteDuration](_.getDuration(_, NANOSECONDS).nanos)

  implicit object ByteSizeConversion extends ConfigConversionAux[ByteSize](StringConversion.g |> ByteSize.apply)

  implicit object TimeoutConversion extends ConfigConversionAux[Timeout](DurationConversion.get)

  implicit object URIConversion extends ConfigConversionAux[URI](StringConversion.g |> (new URI(_)))

  implicit object URLConversion extends ConfigConversionAux[URL](URIConversion.g |> (_.toURL))

  implicit object InetAddressConversion extends ConfigConversionAux[InetAddress](StringConversion.g |> InetAddress.getByName)

  private val HostAndPortRE = """^(.*):(\d*)$""".r
  private val PortOnlyRE = """^(\d+)$""".r

  implicit object InetSocketAddressConversion extends ConfigConversionAux[InetSocketAddress]((cfg, path) =>
    StringConversion.get(cfg, path) match {
      case HostAndPortRE("", port) => new InetSocketAddress(port.toInt)  // ":123"
      case HostAndPortRE(host, "") => new InetSocketAddress(host, 0)  // "abc:"
      case HostAndPortRE(host, port) => new InetSocketAddress(host, port.toInt)  // "abc:123"
      case PortOnlyRE(port) => new InetSocketAddress(port.toInt)  // "123"
      case "" => new InetSocketAddress(0)
      case host => new InetSocketAddress(host, 0)
    }
  )

  implicit object ConfigAtPathConversion extends ConfigConversionAux[Config](_ getConfig _)

  implicit object PathConversion extends ConfigConversionAux[Path](StringConversion.g |> Path.apply)

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
