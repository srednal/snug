package com.srednal.snug

import org.scalatest._
import scala.reflect.runtime.universe._
import scala.reflect.api._
import scala.reflect._

object CaseClassReflectTest {

  case class NoArgs()

  case class Simple(foo: String, bar: Int, baz: List[Boolean])

  trait Thing

  case class More(foo: Int, bar: String, baz: Thing)

  trait HasFoo {
    def foo: String
  }

  case class ExtendsWithFoo(bar: Int, override val foo: String) extends HasFoo

  case class ExtendsNoFoo(bar: Int) extends HasFoo {
    override def foo = bar.toString
  }

  case class SelfRef(foo: String, me: SelfRef) {
    type Useless = Thing
  }

  case class TypeParam[P](p: P, sp: Seq[P], selfish: TypeParam[TypeParam[Seq[Thing]]])

  final class HandMade(val foo: String, val bar: Int) extends Product2[String, Int] {
    override def _1 = foo
    override def _2 = bar
    override def canEqual(that: Any) = that.getClass == classOf[HandMade]
  }

  object HandMade {
    def apply(foo: String, bar: Int) = new HandMade(foo, bar)
  }

  type Y = SelfRef
  type X = TypeParam[Y]


}

class CaseClassReflectTest extends WordSpec with Matchers {
  import CaseClassReflectTest._
  import CaseClassReflect._

  "The CaseClassReflect" should {

    "recognize case classes" in {
      isCaseClass[NoArgs] shouldBe true
    }

    "reflect NoArgs" in {
      caseParamTypes[NoArgs] shouldBe Nil
    }
    "reflect Simple" in {
      caseParamTypes[Simple] shouldBe Seq(
        "foo" -> typeOf[String],
        "bar" -> typeOf[Int],
        "baz" -> typeOf[List[Boolean]])
    }
    "reflect More" in {
      caseParamTypes[More] shouldBe Seq(
        "foo" -> typeOf[Int],
        "bar" -> typeOf[String],
        "baz" -> typeOf[Thing])
    }
    "reflect ExtendsWithFoo" in {
      caseParamTypes[ExtendsWithFoo] shouldBe Seq(
        "bar" -> typeOf[Int],
        "foo" -> typeOf[String])
    }
    "reflect ExtendsNoFoo" in {
      caseParamTypes[ExtendsNoFoo] shouldBe Seq(
        "bar" -> typeOf[Int])
    }
    "reflect SelfRef" in {
      caseParamTypes[SelfRef] shouldBe Seq(
        "foo" -> typeOf[String],
        "me" -> typeOf[SelfRef])
    }
    "reflect TypeParam[Int]" in {
      caseParamTypes[TypeParam[Int]] shouldBe Seq(
        "p" -> typeOf[Int],
        "sp" -> typeOf[Seq[Int]],
        "selfish" -> typeOf[TypeParam[TypeParam[Seq[Thing]]]])
    }
    "reflect TypeParam[Map[String,TypeParam[Thing]]" in {
      caseParamTypes[TypeParam[Map[String, TypeParam[Thing]]]] shouldBe Seq(
        "p" -> typeOf[Map[String, TypeParam[Thing]]],
        "sp" -> typeOf[Seq[Map[String, TypeParam[Thing]]]],
        "selfish" -> typeOf[TypeParam[TypeParam[Seq[Thing]]]])
    }

    "reflect when referenced by a type" in {
      val cpt = caseParamTypes[X]
      cpt shouldBe Seq(
        "p" -> typeOf[Y],
        "sp" -> typeOf[Seq[Y]],
        "selfish" -> typeOf[TypeParam[TypeParam[Seq[Thing]]]])
      assert(cpt(0)._2 =:= typeOf[SelfRef])
      assert(cpt(1)._2 =:= typeOf[Seq[SelfRef]])
    }

    "reflect with wildcard types" in {
      val cpt = caseParamTypes[TypeParam[_]]
      cpt.map(_._1) shouldBe Seq("p", "sp", "selfish")
      assert(cpt(0)._2 <:< typeOf[Any])
      assert(cpt(1)._2 <:< typeOf[Seq[Any]])
      assert(cpt(2)._2 =:= typeOf[TypeParam[TypeParam[Seq[Thing]]]])
    }

    "return Nil for HandMade fake case class" in {
      caseParamTypes[HandMade] shouldBe Nil
    }

    "reflect NoArgs by instance" in {
      val x: Any = NoArgs()
      isCaseClass(x) shouldBe true
      caseParamTypes(x) shouldBe Nil
    }
    "reflect Simple by instance" in {
      val x: Any = Simple("foo", 42, true :: Nil)
      isCaseClass(x) shouldBe true
      caseParamTypes(x) shouldBe Seq(
        "foo" -> typeOf[String],
        "bar" -> typeOf[Int],
        "baz" -> typeOf[List[Boolean]])
    }
    "reflect HasFoo by instance" in {
      val x: HasFoo = ExtendsWithFoo(13, "thirteen")
      isCaseClass(x) shouldBe true
      caseParamTypes(x) shouldBe Seq(
        "bar" -> typeOf[Int],
        "foo" -> typeOf[String])
    }
  }
}
