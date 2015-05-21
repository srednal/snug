package com.srednal.snug

import org.scalatest._

// scalastyle:off magic.number
class MutableOptionTest extends WordSpec with Matchers {

  "A MutableOption" should {

    "be empty" in {
      val mo = MutableOption[BigInt]()

      a[NoSuchElementException] should be thrownBy mo.get

      mo.isEmpty shouldBe true
      mo.isDefined shouldBe false
      mo.asOption shouldBe None

      mo.getOrElse(42) shouldBe 42
      mo.isEmpty shouldBe true // still

      mo.orElse(Some(42)) shouldBe Some(42)
      mo.isEmpty shouldBe true // still

      mo.orNull shouldBe null // scalastyle:ignore null
    }

    "be nonEmpty" in {
      val mo = MutableOption[BigInt](13)

      mo.get shouldBe 13

      mo.isEmpty shouldBe false
      mo.isDefined shouldBe true
      mo.asOption shouldBe Some(13)

      mo.getOrElse(42) shouldBe 13
      mo.orElse(Some(42)) shouldBe Some(13)

      mo.orNull shouldBe 13
    }

    "initialize with a value" in {
      val mo = MutableOption(123)
      mo.isDefined shouldBe true
      mo.get shouldBe 123
    }

    "mutate" in {
      val mo = MutableOption(13)
      mo.get shouldBe 13
      mo set 42
      mo.get shouldBe 42
      mo.clear()
      mo shouldBe empty
    }

    "getOrElseUpdate()" in {
      val mo = MutableOption[Int]()
      mo getOrElseUpdate 13 shouldBe 13
      mo.get shouldBe 13
      mo getOrElseUpdate 42 shouldBe 13
    }

    "foreach()" in {
      val mo = MutableOption[Int]()
      val didIt = MutableOption[Int]()
      mo foreach didIt.set
      didIt shouldBe empty
      mo set 123
      mo foreach didIt.set
      didIt.get shouldBe 123
    }

    "exists()" in {
      val mo = MutableOption[Int]()
      mo exists (_ == 123) shouldBe false
      mo set 13
      mo exists (_ == 123) shouldBe false
      mo set 123
      mo exists (_ == 123) shouldBe true
    }
  }
}
