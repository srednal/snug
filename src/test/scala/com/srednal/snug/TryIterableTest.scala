package com.srednal.snug

import scala.util.{Failure, Success}

class TryIterableTest extends UnitTest {

  "The TryIterable" should {

    "iterate over easy stuff" in {
      new TryIterable("foo" :: "bar" :: "baz" :: Nil).toList shouldBe List(Success("foo"), Success("bar"), Success("baz"))
    }

    "iterate over success/fail things" in {
      case class OddException(i: Int) extends Throwable(s"$i is odd")
      val iter = new Iterable[Int] {
        def iterator: Iterator[Int] = new Iterator[Int] {
          private var i = 0
          def hasNext: Boolean = i < 5
          def next(): Int = {
            val result = i
            i += 1
            if (result % 2 != 0) throw OddException(result)
            result
          }
        }
      }

      new TryIterable(iter).toList shouldBe List(
        Success(0),
        Failure(OddException(1)),
        Success(2),
        Failure(OddException(3)),
        Success(4) // scalastyle:ignore magic.number
      )
    }

    "implicitly make a TryIterable" in {
      import TryIterable._
      List(1, 2, 3).toTryIterable should {
        be(a[TryIterable[_]]) and
          be(a[Iterable[_]]) and
          contain(Success(1)) and
          contain(Success(2)) and
          contain(Success(3))
      }
    }

    "implicitly make a TryStream" in {
      import TryIterable._
      List(1, 2, 3).toTryStream should {
        be(a[LazyList[_]]) and
          contain(Success(1)) and
          contain(Success(2)) and
          contain(Success(3))
      }
    }
  }
}
