package com.srednal.snug

import scala.util.Try
import org.scalatest._
import java.util.concurrent.Callable

class JavaConvertersTest extends WordSpec with Matchers {

  "FunctionAsRunnable" should {
    import com.srednal.snug.JavaConverters.FunctionAsRunnable

    "implicitly convert a func and run it" in {
      var didit = false
      val r: Runnable = () => {didit = true}
      r shouldBe a[Runnable]
      r.run()
      didit shouldBe true
    }
  }

  "FunctionAsCallable" should {
    import com.srednal.snug.JavaConverters.FunctionAsCallable

    "implicitly convert a func and run it" in {
      val r: Callable[String] = () => "done"
      r shouldBe a[Callable[_]]
      r.call() shouldBe "done"
    }

    "throw exceptions" in {
      val r: Callable[String] = () => sys.error("oops")
      r shouldBe a[Callable[_]]
      the[Exception] thrownBy r.call() should have('message ("oops"))
    }

  }
  
  "TryFunctionAsCallable" should {
    import com.srednal.snug.JavaConverters.TryFunctionAsCallable

    "implicitly convert a func and run it" in {
      val r: Callable[String] = () => Try("yep")
      r shouldBe a[Callable[_]]
      r.call() shouldBe "yep"
    }

    "throw exceptions from Failure" in {
      val r: Callable[String] = () => Try[String](sys.error("ack"))
      r shouldBe a[Callable[_]]
      the[Exception] thrownBy r.call() should have('message ("ack"))
    }
  }
}
