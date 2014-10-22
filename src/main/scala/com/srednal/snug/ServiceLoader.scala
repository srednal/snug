package com.srednal.snug

import java.util.{ServiceLoader => JServiceLoader}
import scala.collection.JavaConverters._
import scala.util.{Failure, Try}

object ServicesLoader {

  def apply[S: Manifest]: Stream[S] = {
    val svcs = JServiceLoader.load(manifest[S].runtimeClass, classLoader).asInstanceOf[JServiceLoader[S]].asScala.iterator
    def nxtOpt = Try {svcs.next()}.recoverWith { case e => println(e); Failure(e)}.toOption // todo log it
    def nxt: Stream[Option[S]] = if (svcs.hasNext) nxtOpt #:: nxt else Stream.empty
    nxt.flatten
  }


  def classLoader = Thread.currentThread().getContextClassLoader match {
    case cl: ClassLoader => cl
    case _ => getClass.getClassLoader
  }
}

object ServiceLoader {
  def apply[S: Manifest]: Option[S] = ServicesLoader[S].headOption
}