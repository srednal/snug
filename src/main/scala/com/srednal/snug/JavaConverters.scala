package com.srednal.snug

import java.util.concurrent.Callable

import scala.util.Try

object JavaConverters {

  implicit class FunctionAsRunnable(f: () => Unit) extends Runnable {
    override def run() = f()
  }

  implicit class FunctionAsCallable[A](f: () => A) extends Callable[A] {
    override def call(): A = f()
  }

  implicit class TryFunctionAsCallable[A](f: () => Try[A]) extends FunctionAsCallable[A](() => f().get)
}
