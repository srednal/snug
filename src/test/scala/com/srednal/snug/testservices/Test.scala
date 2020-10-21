package com.srednal.snug
package testservices

object Test {
 val counts = new Array[Int](3)
  def clear(): Unit = {
    counts(0) = 0
    counts(1) = 0
    counts(2) = 0
  }
}
trait Test {
  def count: Int
}

class Test1 extends Test {
  Test.counts(0) += 1
  def count: Int = Test.counts(0)
}

class Test2 extends Test {
  Test.counts(1) += 1
  def count: Int = Test.counts(1)
}

class Test3 extends Test {
  Test.counts(2) += 1
  def count: Int = Test.counts(2)
}

class TestErr1 extends Test {
  assert(false)
  def count: Int = 0
}

class TestErr2 extends Test {
  assert(false)
  def count: Int = 0
}

class TestErr3 extends Test {
  assert(false)
  def count: Int = 0
}

trait NotThere
