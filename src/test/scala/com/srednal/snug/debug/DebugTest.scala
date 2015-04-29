package com.srednal.snug.debug

import java.util.concurrent.atomic.AtomicReference
import com.srednal.snug.debug
import org.scalatest._

class DebugTest extends WordSpec with Matchers with debug.Debug {

  class X {
    val debugged  = new AtomicReference[String]()  // let someone else be responsible for mutable stuff
    def debug(s: String) = debugged set s
  }


  val x = "the value"

  "the debug macro" should {
    "debug" in {
      val logger = new X

      val y = "the other"
      debug(x, y) // macro expands to: logger.debug("Woot.this.x = the value, Woot.this.y = the other")

      logger.debugged.get shouldBe "DebugTest.this.x = the value, y = the other"

    }
  }
}
