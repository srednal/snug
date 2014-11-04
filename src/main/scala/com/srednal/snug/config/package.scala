package com.srednal.snug

import com.srednal.snug.CaseClassReflect._
import com.typesafe.config._
import com.typesafe.config.ConfigValueType._
import scala.reflect.runtime.universe._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.collection.JavaConverters._

package object config {

  /** Global/default configuration */
  val config = ConfigFactory.load()

  /**
   * Pimp a Config with some helpful conversions
   */
  implicit class RichConfig(val cfg: Config) {
    /** Fetch the sub-config at this path */
    def apply(path: String): Config = cfg.getConfig(path)

    /**
     * Fetch the value at path converted to the type A.
     *
     * A can be any of:
     * - String (for any config type).
     * - Int, Long, Double, etc (for config numbers).
     * - Boolean (for config booleans).
     * - Duration, FiniteDuration, or Timeout (for config numbers or parsible strings).
     * - List, Seq, Iterable, Traversable, or Set of any valid A type (for config lists).
     * - Case Class, for config objects where the keys of path match the args of the case class primary constructor (and are some valid A type).
     */
    def as[A: TypeTag](path: String): A =
      try asTypeOf(typeOf[A], path).asInstanceOf[A]
      catch {
        case NonFatal(e) if cfg.hasPath(path) =>
          throw new IllegalArgumentException(s"Error fetching config path $path as ${typeOf[A]} from ${cfg.getValue(path).origin().description()}", e)
      }

    private def asTypeOf(tpe: Type, path: String): Any /* <: tpe */ = tpe match {

      // we can make anything into a string (object and list are ugly, but you asked for it)
      case StringType => cfg.getValue(path).unwrapped().toString

      case t if t <:< option_Type => if (cfg.hasPath(path)) Some(asTypeOf(t.typeArgs.head, path)) else None

      case _ =>
        val configValue = cfg.getValue(path)

        (tpe, configValue, configValue.valueType()) match {

          // scala Duration and HOCON have slightly different string representations.
          // Could parse with Duration(v.as[String]), but keep the HOCON semantics:
          case (t, _, STRING | NUMBER) if t <:< durationType => cfg.getDuration(path, NANOSECONDS).nanos

          case (TimeoutType, _, STRING | NUMBER) => Timeout(as[FiniteDuration](path))

          // support Set specifically
          case (t, v: ConfigList, _) if t <:< set_Type => asListOf(t.typeArgs.head, v).toSet

          // other Traversables will be instances of List (so List, Seq, Iterable, Traversable will be ok)
          case (t, v: ConfigList, _) if t <:< traversable_Type => asListOf(t.typeArgs.head, v)

          // case class mapping
          case (t, v: ConfigObject, _) if typeIsCaseClass(t) =>
            val vCfg: RichConfig = v.toConfig
            // fetch a config value for each case class param
            createForType(t)(caseParamTypesForType(t) map { case (n, p) => vCfg.asTypeOf(p, n)})

          case (_, v, _) => v.unwrapped() // This will handle numbers and boolean ok.  For other stuff: wing it and see...
        }
    }

    private def asListOf(of: Type, v: ConfigList): List[_ /* <: of */ ] =
      v.asScala.toList map (_.atKey("X")) map (new RichConfig(_)) map (_.asTypeOf(of, "X"))
  }

  private val StringType = typeOf[String]
  private val TimeoutType = typeOf[Timeout]

  private val durationType = typeOf[Duration]
  private val option_Type = typeOf[Option[_]]
  private val set_Type = typeOf[Set[_]]
  private val traversable_Type = typeOf[Traversable[_]]

}
