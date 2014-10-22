package com.srednal.snug

import org.scalatest._

class FooTest extends WordSpec with Matchers {

  "Foo" should {

    "foo" in {

      new Foo().hello shouldBe "world"

    }
  }
}
