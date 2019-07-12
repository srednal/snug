package com.srednal.snug

import scala.util.Try

object TryIterable {
  implicit class AsTryIterable[A](underlying: Iterable[A]) {
    def toTryIterable: Iterable[Try[A]] = new TryIterable(underlying)
    def toTryStream: LazyList[Try[A]] = new TryIterable(underlying).tryLazyList
  }
}

/**
 * A Lazy iterable with the underlying's next() wrapped in Try.
 * When you have a lazy iterable (i.e. a LazyList) whose next() fetch might have
 * error side-effects (i.e. constructing objects, etc).
 */
class TryIterable[A](underlying: Iterable[A]) extends Iterable[Try[A]] {
  private def asLazyList(ui: Iterator[A]): LazyList[Try[A]] =
    if (ui.hasNext) Try( ui.next()) #:: asLazyList(ui) else LazyList.empty
  lazy val tryLazyList: LazyList[Try[A]] = asLazyList(underlying.iterator)
  override def iterator: Iterator[Try[A]] = tryLazyList.iterator
}

