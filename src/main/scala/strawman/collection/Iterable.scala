package strawman.collection

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag
import scala.{Any, AnyVal, Array, Boolean, Int, StringContext, Unit}
import java.lang.String

import strawman.collection.mutable.{ArrayBuffer, Iterator, StringBuilder}

/** Base trait for generic collections */
trait Iterable[+A] extends IterableLike[A, Iterable] {
  /** The collection itself */
  protected def coll: this.type = this

  /** Iterator can be used only once */
  def iterator(): Iterator[A]

  /** Operations based on the underlying `Iterator` */
  final def iterating: Iterating[A] = new Iterating(this)

}

/** Low-level operations based on the underlying `Iterator` of an `Iterable` */
class Iterating[+A](val iterable: Iterable[A]) extends AnyVal {

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val it = iterable.iterator()
    var b = z
    while (it.hasNext) {
      b = op(b, it.next())
    }
    b
  }

  def foldRight[B](z: B)(op: (A, B) => B): B = {
    val it = iterable.iterator()
    def loop(): B = if (it.hasNext) op(it.next(), loop()) else z
    loop()
  }

  def foreach(f: A => Unit): Unit = {
    val it = iterable.iterator()
    while (it.hasNext) f(it.next())
  }

  def indexWhere(p: A => Boolean): Int = {
    val it = iterable.iterator()
    var i = 0
    while (it.hasNext) {
      if (p(it.next())) return i
      i += 1
    }
    -1
  }

  def length: Int = {
    val it = iterable.iterator()
    var len = 0
    while (it.hasNext) { len += 1; it.next() }
    len
  }

  def filter(p: A => Boolean): Iterator[A] = {
    val it = iterable.iterator()
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
    val it = iterable.iterator()
    new Iterator[B] {
      def hasNext = it.hasNext
      def next() = f(it.next())
    }
  }

  def flatMap[B](f: A => Iterable[B]): Iterator[B] = {
    val it = iterable.iterator()
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

  def ++ [B >: A](bs: Iterable[B]): Iterator[B] = {
    val it = iterable.iterator()
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
    val it = iterable.iterator()
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
    val it = iterable.iterator()
    var i = 0
    while (i < n && it.hasNext) {
      it.next()
      i += 1
    }
    it
  }

  def zip[B](that: Iterable[B]): Iterator[(A, B)] = {
    val it = iterable.iterator()
    new Iterator[(A, B)] {
      val thatIterator = that.iterator()
      def hasNext = it.hasNext && thatIterator.hasNext
      def next() = (it.next(), thatIterator.next())
    }
  }

}

/** Base trait for Iterable operations
  *
  *  VarianceNote
  *  ============
  *
  *  We require that for all child classes of Iterable the variance of
  *  the child class and the variance of the `C` parameter passed to `IterableLike`
  *  are the same. We cannot express this since we lack variance polymorphism. That's
  *  why we have to resort at some places to write `C[A @uncheckedVariance]`.
  *
  */
trait IterableLike[+A, +C[X] <: Iterable[X]]
  extends FromIterable[C]
    with IterableOps[A]
    with IterableMonoTransforms[A, C[A @uncheckedVariance]] // sound bcs of VarianceNote
    with IterablePolyTransforms[A, C] {

  /** Create a collection of type `C[A]` from the elements of `coll`, which has
    *  the same element type as this collection. Overridden in StringOps and ArrayOps.
    */
  protected[this] def fromIterableWithSameElemType(coll: Iterable[A]): C[A] = fromIterable(coll)
}

/** Base trait for instances that can construct a collection from an iterable */
trait FromIterable[+C[X] <: Iterable[X]] {
  def fromIterable[B](it: Iterable[B]): C[B]
}

/** Base trait for companion objects of collections */
trait IterableFactory[+C[X] <: Iterable[X]] extends FromIterable[C] {
  def empty[X]: C[X] = fromIterable(View.Empty)
  def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))
}

/** Operations over iterables. No operation defined here is generic in the
  *  type of the underlying collection.
  */
trait IterableOps[+A] extends Any {
  protected def coll: Iterable[A]
  private def iterator(): Iterator[A] = coll.iterator()

  /** Apply `f` to each element for tis side effects */
  def foreach(f: A => Unit): Unit = coll.iterating.foreach(f)

  /** Fold left */
  def foldLeft[B](z: B)(op: (B, A) => B): B = coll.iterating.foldLeft(z)(op)

  /** Fold right */
  def foldRight[B](z: B)(op: (A, B) => B): B = coll.iterating.foldRight(z)(op)

  /** The index of the first element in this collection for which `p` holds. */
  def indexWhere(p: A => Boolean): Int = coll.iterating.indexWhere(p)

  /** Is the collection empty? */
  def isEmpty: Boolean = !iterator().hasNext

  /** The first element of the collection. */
  def head: A = iterator().next()

  /** The number of elements in this collection, if it can be cheaply computed,
    *  -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
    */
  def knownSize: Int = -1

  /** The number of elements in this collection. Does not terminate for
    *  infinite collections.
    */
  def size: Int = if (knownSize >= 0) knownSize else coll.iterating.length

  /** A view representing the elements of this collection. */
  def view: View[A] = View.fromIterator(iterator())

  /** Given a collection factory `fi` for collections of type constructor `C`,
    *  convert this collection to one of type `C[A]`. Example uses:
    *
    *      xs.to(List)
    *      xs.to(ArrayBuffer)
    */
  def to[C[X] <: Iterable[X]](fi: FromIterable[C]): C[A @uncheckedVariance] =
  // variance seems sound because `to` could just as well have been added
  // as a decorator. We should investigate this further to be sure.
    fi.fromIterable(coll)

  /** Convert collection to array. */
  def toArray[B >: A: ClassTag]: Array[B] =
    if (knownSize >= 0) copyToArray(new Array[B](knownSize), 0)
    else ArrayBuffer.fromIterable(coll).toArray[B]

  /** Copy all elements of this collection to array `xs`, starting at `start`. */
  def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = {
    var i = start
    val it = iterator()
    while (it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
    xs
  }

  /** The class name of this collection. To be used for converting to string.
    *  Collections generally print like this:
    *
    *       <className>(elem_1, ..., elem_n)
    */
  def className = getClass.getName

  /** A string showing all elements of this collection, separated by string `sep`. */
  def mkString(sep: String): String = {
    var first: Boolean = true
    val b = new StringBuilder()
    foreach { elem =>
      if (!first) b ++= sep
      first = false
      b ++= String.valueOf(elem)
    }
    b.result
  }

  override def toString = s"$className(${mkString(", ")})"
}

/** Type-preserving transforms over iterables.
  *  Operations defined here return in their result iterables of the same type
  *  as the one they are invoked on.
  */
trait IterableMonoTransforms[+A, +Repr] extends Any {
  protected def coll: Iterable[A]
  protected[this] def fromIterableWithSameElemType(coll: Iterable[A]): Repr

  /** All elements satisfying predicate `p` */
  def filter(p: A => Boolean): Repr = fromIterableWithSameElemType(View.Filter(coll, p))

  /** A pair of, first, all elements that satisfy prediacte `p` and, second,
    *  all elements that do not. Interesting because it splits a collection in two.
    *
    *  The default implementation provided here needs to traverse the collection twice.
    *  Strict collections have an overridden version of `partition` in `Buildable`,
    *  which requires only a single traversal.
    */
  def partition(p: A => Boolean): (Repr, Repr) = {
    val pn = View.Partition(coll, p)
    (fromIterableWithSameElemType(pn.left), fromIterableWithSameElemType(pn.right))
  }

  /** A collection containing the first `n` elements of this collection. */
  def take(n: Int): Repr = fromIterableWithSameElemType(View.Take(coll, n))

  /** The rest of the collection without its `n` first elements. For
    *  linear, immutable collections this should avoid making a copy.
    */
  def drop(n: Int): Repr = fromIterableWithSameElemType(View.Drop(coll, n))

  /** The rest of the collection without its first element. */
  def tail: Repr = drop(1)
}

/** Transforms over iterables that can return collections of different element types.
  */
trait IterablePolyTransforms[+A, +C[A]] extends Any {
  protected def coll: Iterable[A]
  def fromIterable[B](coll: Iterable[B]): C[B]

  /** Map */
  def map[B](f: A => B): C[B] = fromIterable(View.Map(coll, f))

  /** Flatmap */
  def flatMap[B](f: A => Iterable[B]): C[B] = fromIterable(View.FlatMap(coll, f))

  /** Concatenation */
  def ++[B >: A](xs: Iterable[B]): C[B] = fromIterable(View.Concat(coll, xs))

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B](xs: Iterable[B]): C[(A @uncheckedVariance, B)] = fromIterable(View.Zip(coll, xs))
  // sound bcs of VarianceNote
}
