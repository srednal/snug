package com.srednal.snug

import akka.util.Timeout
import com.typesafe.config._
import com.typesafe.config.ConfigValueType._
import com.srednal.snug.CaseClassReflect._
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal

package object config {

  val config = ConfigFactory.load()

  implicit class RichConfig(val cfg: Config) {
    def apply(path: String): Config = cfg.getConfig(path)
    def as[A: TypeTag](path: String): A =
      try asType(typeOf[A])(path).asInstanceOf[A]
      catch {
        case NonFatal(e) if cfg.hasPath(path) => throw new IllegalArgumentException(cfg.getValue(path).origin().description(), e)
      }

    private def asType(tpe: Type)(path: String): Any = tpe match {

      case StringType => cfg.getValue(path).unwrapped().toString

      case t if t <:< typeOf[Option[_]] => if (cfg.hasPath(path)) Some(asType(t.typeArgs.head)(path)) else None

      case _ =>
        val configValue = cfg.getValue(path)

        (tpe, configValue, configValue.valueType()) match {

          case (IntType, v, NUMBER) => v.unwrapped().asInstanceOf[Number].intValue()
          case (LongType, v, NUMBER) => v.unwrapped().asInstanceOf[Number].longValue()
          case (DoubleType, v, NUMBER) => v.unwrapped().asInstanceOf[Number].doubleValue()

          case (BooleanType, v, BOOLEAN) => v.unwrapped().asInstanceOf[Boolean]


          // scala Duration and HOCON have slightly different string representations.
          // Could parse with Duration(v.as[String]), but keep the HOCON semantics:
          case (t, v, STRING) if t <:< typeOf[Duration] => v.atKey("X").getDuration("X", NANOSECONDS).nanos

          case (t, v, STRING) if t <:< typeOf[Timeout] => Timeout(as[FiniteDuration](path))

          // Container types (recurse):

          // support specific types of collections so the builders are created properly
          case (t, v, LIST) if t <:< typeOf[Set[_]] => asListOf(t.typeArgs.head)(v.asInstanceOf[ConfigList]).toSet
          case (t, v, LIST) if t <:< typeOf[Traversable[_]] => asListOf(t.typeArgs.head)(v.asInstanceOf[ConfigList])

          case (t, v, OBJECT) if typeIsCaseClass(t) => createForType(t) {
            val vCfg = new RichConfig(v.asInstanceOf[ConfigObject].toConfig)
            caseParamTypesForType(t) map {
              // config value for each case class param
              case (n, p) => vCfg.asType(p)(n)
            }
          }

          case (_, v, _) => v.unwrapped() // wing it
        }

    }

    private def asListOf(of: Type)(v: ConfigList) = v.asScala.toList map { x => new RichConfig(x.atKey("X")).asType(of)("X")}
  }

  private val StringType = typeOf[String]
  private val IntType = typeOf[Int]
  private val LongType = typeOf[Long]
  private val DoubleType = typeOf[Double]
  private val BooleanType = typeOf[Boolean]

}
