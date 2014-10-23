package com.srednal.snug

import com.srednal.snug.TryIterable._
import com.srednal.snug.log.Logger
import java.util.{ServiceLoader => JServiceLoader}
import scala.collection.JavaConverters._
import scala.reflect._
import scala.util.Failure

/**
 * Implementations of MyService (generally a trait) can be listed in META-INF/services/com.srednal.snug.MyService
 * Implementations must have a no-args constructor and otherwise be able to configure themselves (see com.srednal.snug.config).
 * If further configuration or runtime information is needed, create a factory or builder as a service.
 */
object ServicesLoader {

  /**
   * Fetch the service implementations.
   * This iterable is lazy (it is a Stream) and will not instantiate any service implementation until accessed
   * (except that head is initially instantiated).
   * Any services which fail creation will have the exception logged and will not appear in this iterable.
   * Each call to this method will instantiate a new set of service instances.
   */
  def apply[S](implicit ct: ClassTag[S], logger: Logger = Logger.Silent): Iterable[S] =
    services[S].toTryStream.map(_.recoverWith {
      case e =>
        logger.error(s"Error loading service for $ct", e)
        Failure(e)
    }.toOption).flatten

  def services[S: ClassTag]: Iterable[S] =
    JServiceLoader.load(classTag[S].runtimeClass.asInstanceOf[Class[S]], classLoader).asScala

  def classLoader = Thread.currentThread().getContextClassLoader match {
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
  def apply[S](implicit ct: ClassTag[S], logger: Logger = Logger.Silent): Option[S] = ServicesLoader[S].headOption
}