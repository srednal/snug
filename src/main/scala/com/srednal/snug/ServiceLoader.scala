package com.srednal.snug

import com.srednal.snug.TryIterable._
import java.util.{ ServiceLoader => JServiceLoader }
import scala.jdk.CollectionConverters._
import scala.reflect._
import scala.util.Failure
import scala.util.control.NonFatal
import com.typesafe.scalalogging.LazyLogging

/**
 * Implementations of MyService (generally a trait) can be listed in META-INF/services/com.srednal.snug.MyService
 * Implementations must have a no-args constructor and otherwise be able to configure themselves (see com.srednal.snug.config).
 * If further configuration or runtime information is needed, create a factory or builder as a service.
 */
object ServicesLoader extends LazyLogging {

  /**
   * Fetch the service implementations.
   * This iterable is lazy (it is a Stream) and will not instantiate any service implementation until accessed
   * (except that head is initially instantiated).
   * Any services which fail creation will have the exception logged and will not appear in this iterable.
   * Each call to this method will instantiate a new set of service instances.
   */
  def apply[S](implicit ct: ClassTag[S]): Iterable[S] =
    services[S].toTryStream.flatMap(_.recoverWith {
      case NonFatal(e) =>
        logger.error(s"Error loading service for $ct", e)
        Failure(e)
    }.toOption
    )

  def services[S](implicit ct: ClassTag[S]): Iterable[S] =
    JServiceLoader.load(ct.runtimeClass.asInstanceOf[Class[S]], classLoader).asScala

  def classLoader: ClassLoader = Thread.currentThread().getContextClassLoader match {
    case cl: ClassLoader => cl
    case _ => getClass.getClassLoader
  }
}

// singular
object ServiceLoader {
  /**
   * Fetch a service implementations.
   * The first (successful) service is returned.
   * Generally expected to be used when there should be only one useful service of a given type.
   * Each call to this method will instantiate a new instance of the service.
   */
  def apply[S](implicit ct: ClassTag[S]): Option[S] = ServicesLoader[S].headOption
}
