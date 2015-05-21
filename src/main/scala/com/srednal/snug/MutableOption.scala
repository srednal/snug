package com.srednal.snug

import java.util.concurrent.atomic.AtomicReference


object MutableOption {
  def apply[A]() = new MutableOption[A]()
  def apply[A](value: A) = new MutableOption[A](value)
}

final class MutableOption[A] private(initialValue: A) {
  private def this() = this(null.asInstanceOf[A]) // scalastyle:ignore null

  private val ref = new AtomicReference[A](initialValue)

  def set(newValue: A): Unit = ref set newValue

  def clear(): Unit = set(null.asInstanceOf[A]) // scalastyle:ignore null

  def asOption = Option(ref.get)

  def get: A = asOption getOrElse {throw new NoSuchElementException("MutableOption.None")}

  def isEmpty: Boolean = asOption.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def isDefined: Boolean = nonEmpty

  def getOrElse[B >: A](default: => B): B = asOption getOrElse default

  def orElse[B >: A](alternative: => Option[B]): Option[B] = asOption orElse alternative

  def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = asOption.orNull

  def foreach[U](f: A => U): Unit = asOption foreach f

  def exists(p: A => Boolean): Boolean = asOption exists p

  def getOrElseUpdate(newValue: => A): A = getOrElse {
    val v = newValue
    set(v)
    v
  }

}
