package com.srednal.snug

import com.srednal.snug.CaseClassReflect._
import com.typesafe.config._
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
     * - Duration, FiniteDuration, or Timeout (when config can parse it as a duration).
     * - Traversable[A], Set[A], or Array[A] (for config lists).
     * = Option[A] (None where no config at the path).
     * - Case Class, for config objects where the keys of path match the args of the case class primary constructor (and each are some valid A type).
     */
    def as[A: TypeTag](path: String): A =
      try asTypeOf(typeOf[A], path).asInstanceOf[A]
      catch {
        case NonFatal(e) =>
          throw new IllegalArgumentException(s"Error fetching config path $path as ${typeOf[A]} from ${descr(path)}", e)
      }

    private def descr(path:String) = if (cfg.hasPath(path)) cfg.getValue(path).origin().description() else cfg.origin().description()

    private def asTypeOf(tpe: Type, path: String): Any /* <: tpe */ = {
      // handle option specifically, as it needs to check hasPath, getValue may error
      if (tpe <:< option_Type) if (cfg.hasPath(path)) Some(asTypeOf(tpe.typeArgs.head, path)) else None
      else (tpe, cfg.getValue(path)) match {

          case (StringType, v) => v.unwrapped().toString

          // scala Duration and HOCON have slightly different string representations.
          // Could parse with Duration(v.as[String]), but keep the HOCON semantics:
          case (t, _) if t <:< durationType => cfg.getDuration(path, NANOSECONDS).nanos

          case (TimeoutType, _) => Timeout(as[FiniteDuration](path))

          // support Set, Array specifically
          case (t@TypeRef(_, _, arg :: Nil), v: ConfigList) if t <:< set_Type => asListOf(arg, v).toSet

          case (t@TypeRef(_, _, arg :: Nil), v: ConfigList) if t <:< array_Type => asListOf(arg, v).toArray

          // other Traversables will be instances of List (so List, Seq, Iterable, Traversable will be ok)
          case (t@TypeRef(_, _, arg :: Nil), v: ConfigList) if t <:< traversable_Type => asListOf(arg, v)

          // case class mapping
          case (t, v: ConfigObject) if typeIsCaseClass(t) =>
            val vCfg: RichConfig = v.toConfig
            // fetch a config value for each case class param
            createForType(t)(caseParamTypesForType(t) map { case (n, p) => vCfg.asTypeOf(p, n)})

          case (_, v) => v.unwrapped() // This will handle numbers and boolean ok.  For other stuff: wing it and see...
        }
    }

    private def asListOf(tpe: Type, v: ConfigList): List[_ /* <: tpe */ ] =
      v.asScala.toList map (_.atKey("X")) map (new RichConfig(_)) map (_.asTypeOf(tpe, "X"))
  }

  private val StringType = typeOf[String]
  private val TimeoutType = typeOf[Timeout]

  private val durationType = typeOf[Duration]
  private val option_Type = typeOf[Option[_]]
  private val set_Type = typeOf[Set[_]]
  private val array_Type = typeOf[Array[_]]
  private val traversable_Type = typeOf[Traversable[_]]

}
