package strawman.collection

import strawman.collection.mutable.Iterator
import scala.{AnyVal, Boolean, Int, Unit}

trait IterableOnce[+A] {
  /** Iterator can be used only once */
  def iterator(): Iterator[A]

  final def iterating: IterableOnce.Iterating[A] = new IterableOnce.Iterating(this)
}

object IterableOnce {

  /**
    * High-level operations based on the underlying `Iterator`.
    */
  class Iterating[+A](val iterableOnce: IterableOnce[A]) extends AnyVal {

    def foldLeft[B](z: B)(op: (B, A) => B): B = {
      val it = iterableOnce.iterator()
      var b = z
      while (it.hasNext) {
        b = op(b, it.next())
      }
      b
    }

    def foldRight[B](z: B)(op: (A, B) => B): B = {
      val it = iterableOnce.iterator()
      def loop(): B = if (it.hasNext) op(it.next(), loop()) else z
      loop()
    }

    def foreach(f: A => Unit): Unit = {
      val it = iterableOnce.iterator()
      while (it.hasNext) f(it.next())
    }

    def indexWhere(p: A => Boolean): Int = {
      val it = iterableOnce.iterator()
      var i = 0
      while (it.hasNext) {
        if (p(it.next())) return i
        i += 1
      }
      -1
    }

    def length: Int = {
      val it = iterableOnce.iterator()
      var len = 0
      while (it.hasNext) { len += 1; it.next() }
      len
    }

    def filter(p: A => Boolean): Iterator[A] = {
      val it = iterableOnce.iterator()
      new Iterator[A] {
        private var hd: A = _
        private var hdDefined: Boolean = false

        def hasNext: Boolean = hdDefined || {
          do {
            if (!it.hasNext) return false
            hd = it.next()
          } while (!p(hd))
          hdDefined = true
          true
        }

        def next() =
          if (hasNext) {
            hdDefined = false
            hd
          }
          else Iterator.empty.next()
      }
    }

    def map[B](f: A => B): Iterator[B] = {
      val it = iterableOnce.iterator()
      new Iterator[B] {
        def hasNext = it.hasNext
        def next() = f(it.next())
      }
    }

    def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = {
      val it = iterableOnce.iterator()
      new Iterator[B] {
        private var myCurrent: Iterator[B] = Iterator.empty
        private def current = {
          while (!myCurrent.hasNext && it.hasNext)
            myCurrent = f(it.next()).iterator()
          myCurrent
        }
        def hasNext = current.hasNext
        def next() = current.next()
      }
    }

    def ++ [B >: A](bs: IterableOnce[B]): Iterator[B] = {
      val it = iterableOnce.iterator()
      new Iterator[B] {
        private var myCurrent: Iterator[B] = it
        private var first = true
        private def current = {
          if (!myCurrent.hasNext && first) {
            myCurrent = bs.iterator()
            first = false
          }
          myCurrent
        }
        def hasNext = current.hasNext
        def next() = current.next()
      }
    }

    def take(n: Int): Iterator[A] = {
      val it = iterableOnce.iterator()
      new Iterator[A] {
        private var i = 0
        def hasNext = it.hasNext && i < n
        def next() =
          if (hasNext) {
            i += 1
            it.next()
          }
          else Iterator.empty.next()
      }
    }

    def drop(n: Int): Iterator[A] = {
      val it = iterableOnce.iterator()
      var i = 0
      while (i < n && it.hasNext) {
        it.next()
        i += 1
      }
      it
    }

    def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = {
      val it = iterableOnce.iterator()
      new Iterator[(A, B)] {
        val thatIterator = that.iterator()
        def hasNext = it.hasNext && thatIterator.hasNext
        def next() = (it.next(), thatIterator.next())
      }
    }

  }

}