package com.srednal.snug

import java.util.concurrent.atomic.AtomicReference


object MutableOption {
  def apply[A]() = new MutableOption[A]()
  def apply[A](value: A) = new MutableOption[A](value)
}

final class MutableOption[A] private(initialValue: Option[A]) {
  private def this() = this(None)
  private def this(a: A) = this(Option(a))

  private val ref = new AtomicReference[Option[A]](initialValue)

  def set(newValue: A): Unit = ref set Option(newValue)

  def clear(): Unit = ref set None

  def asOption: Option[A] = ref.get

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
