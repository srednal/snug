package com.srednal.snug

import scala.util.Try

object TryIterable {
  implicit class AsTryIterable[A](underlying: Iterable[A]) {
    def toTryIterable: Iterable[Try[A]] = new TryIterable(underlying)
    def toTryStream: Stream[Try[A]] = new TryIterable(underlying).tryStream
  }
}

/**
 * A Lazy iterable with the underlying's next() wrapped in Try.
 * When you have a lazy iterable (i.e. a Stream-like thing) whose next() fetch might have
 * error side-effects (i.e. constructing objects, etc).
 */
class TryIterable[A](underlying: Iterable[A]) extends Iterable[Try[A]] {
  def asStream(ui: Iterator[A]): Stream[Try[A]] = if (ui.hasNext) Try( ui.next()) #:: asStream(ui) else Stream.empty
  lazy val tryStream: Stream[Try[A]] = asStream(underlying.iterator)
  override def iterator = tryStream.iterator
}

