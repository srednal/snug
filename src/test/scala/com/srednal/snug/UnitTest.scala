package com.srednal.snug

import scala.reflect.ClassTag
import org.mockito.{ArgumentCaptor, Mockito}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait UnitTest extends AnyWordSpec with Matchers {

  /** Transition for symbol literal have matcher: `foo should have( prop"xyzzy"("plugh")` */
  implicit class HaveGen(sc: StringContext) {
    def prop(): HavePropertyMatcherGenerator = Symbol(sc.raw())
  }

  def mock[M](implicit tag: ClassTag[M]): M = Mockito.mock(tag.runtimeClass.asInstanceOf[Class[M]])

  def captor[A](implicit tag: ClassTag[A]): ArgumentCaptor[A] = ArgumentCaptor.forClass[A, A](tag.runtimeClass.asInstanceOf[Class[A]])

}
