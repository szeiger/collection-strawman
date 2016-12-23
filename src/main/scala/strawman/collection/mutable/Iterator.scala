package strawman.collection.mutable

import scala.{Boolean, Int, Unit, Nothing, NoSuchElementException}
import strawman.collection.IndexedView

/** A core Iterator class */
trait Iterator[+A] { self =>
  def hasNext: Boolean
  def next(): A
}

object Iterator {
  val empty: Iterator[Nothing] = new Iterator[Nothing] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next on empty iterator")
  }
  def apply[A](xs: A*): Iterator[A] = new IndexedView[A] {
    val length = xs.length
    def apply(n: Int) = xs(n)
  }.iterator()
}
