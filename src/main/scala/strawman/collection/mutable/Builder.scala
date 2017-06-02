package strawman.collection.mutable

import scala.{Any, Boolean, Char, Unit}
import java.lang.String

import strawman.collection.{BuildFrom, IterableFactory, IterableFactoryWithBuilder, IterableOnce, SortedIterableFactory, StrictBuildFrom}

import scala.math.Ordering

/** Base trait for collection builders */
trait Builder[-A, +To] extends Growable[A] { self =>

  /** Clears the contents of this builder.
   *  After execution of this method the builder will contain no elements.
   */
  def clear(): Unit

  /** Result collection consisting of all elements appended so far. */
  def result(): To

  /** A builder resulting from this builder my mapping the result using `f`. */
  def mapResult[NewTo](f: To => NewTo) = new Builder[A, NewTo] {
    def add(x: A): this.type = { self += x; this }
    def clear(): Unit = self.clear()
    override def addAll(xs: IterableOnce[A]): this.type = { self ++= xs; this }
    def result(): NewTo = f(self.result())
  }
}

object Builder {
  /** Get a proper builder for an IterableFactoryWithBuilder, otherwise a Builder that uses an intermediate
    * ArrayBuffer to store the elements. */
  def from[A, CC[_]](fact: IterableFactory[CC]): Builder[A, CC[A]] = fact match {
    case fact: IterableFactoryWithBuilder[CC] => fact.newBuilder[A]()
    case fact => new ArrayBuffer[A]().mapResult(fact.fromIterable _)
  }

  /** Get a proper builder for a SortedIterableFactoryWithBuilder, otherwise a Builder that uses an intermediate
    * ArrayBuffer to store the elements. */
  def from[A : Ordering, CC[_]](fact: SortedIterableFactory[CC]): Builder[A, CC[A]] = fact match {
    case fact: IterableFactoryWithBuilder[CC] => fact.newBuilder[A]()
    case fact => new ArrayBuffer[A]().mapResult(fact.sortedFromIterable[A] _)
  }

  /** Get a proper builder for a StrictBuildFrom, otherwise a Builder that uses an intermediate
    * ArrayBuffer to store the elements. */
  def from[From, A, C](bf: BuildFrom[From, A, C], from: From): Builder[A, C] = bf match {
    case bf: StrictBuildFrom[From, A, C] => bf.newBuilder(from)
    case bf => new ArrayBuffer[A]().mapResult(bf.fromSpecificIterable(from) _)
  }
}

class StringBuilder extends Builder[Char, String] {
  private val sb = new java.lang.StringBuilder

  def add(x: Char) = { sb.append(x); this }

  def clear() = sb.setLength(0)

  /** Overloaded version of `addAllInPlace` that takes a string */
  def addAllInPlace(s: String): this.type = { sb.append(s); this }

  /** Alias for `addAllInPlace` */
  def ++= (s: String): this.type = addAllInPlace(s)

  def result() = sb.toString

  override def toString = result()
}
