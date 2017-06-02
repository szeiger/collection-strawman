package strawman
package collection

import scala.language.implicitConversions

import strawman.collection.mutable.{ArrayBuffer, Builder}

import scala.{Any, Int, Nothing, Ordering}
import scala.annotation.unchecked.uncheckedVariance


/** Builds a collection of type `C` from elements of type `A` when a source collection of type `From` is available.
  * Implicit instances of `BuildFrom` are available for all collection types.
  *
  * @tparam From Type of source collection
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait BuildFrom[-From, -A, +C] extends Any {
  def fromSpecificIterable(from: From)(it: Iterable[A]): C
}

object BuildFrom {
  /** Build the source collection type from an IterableOps */
  implicit def buildFromIterableOps[C[X] <: IterableOps[X, C, _], A, E]: BuildFrom[C[A], E, C[E]] = new BuildFrom[C[A], E, C[E]] {
    //TODO: Reuse a prototype instance
    def fromSpecificIterable(from: C[A])(it: Iterable[E]): C[E] = from.iterableFactory.fromIterable(it)
  }

  /** Build the source collection type from an Iterable with SortedOps */
  implicit def buildFromSortedOps[C[X] <: Iterable[X] with SortedOps[X, C[X], C], A, E : Ordering]: BuildFrom[C[A], E, C[E]] = new BuildFrom[C[A], E, C[E]] {
    def fromSpecificIterable(from: C[A])(it: Iterable[E]): C[E] = from.sortedIterableFactory.fromSpecificIterable(it)
  }
}

/** A more specific `BuildFrom` for strict target collection types which can provide a `Builder`.
  * Note that a `Builder` can be obtained for any `BuildFrom` via `Builder.from`.
  */
trait StrictBuildFrom[-From, -A, +C] extends Any with BuildFrom[From, A, C] {
  def newBuilder(from: From): Builder[A, C]
}

object StrictBuildFrom {
  /** Build the source collection type from a strict IterableOps */
  implicit def strictBuildFromIterableOps[C[X] <: IterableOps[X, C, _] with Buildable[X, C[X]], A, E]: StrictBuildFrom[C[A], E, C[E]] = new StrictBuildFrom[C[A], E, C[E]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: C[A]): Builder[E, C[E]] = from.iterableFactory.asInstanceOf[IterableFactoryWithBuilder[C]].newBuilder[E]()
    def fromSpecificIterable(from: C[A])(it: Iterable[E]): C[E] = from.iterableFactory.fromIterable(it)
  }

  /** Build the source collection type from a strict Iterable with SortedOps */
  implicit def strictBuildFromSortedOps[C[X] <: Iterable[X] with SortedOps[X, C[X], C] with Buildable[X, C[X]], A, E : Ordering]: StrictBuildFrom[C[A], E, C[E]] = new StrictBuildFrom[C[A], E, C[E]] {
    def newBuilder(from: C[A]): Builder[E, C[E]] = from.sortedIterableFactory.asInstanceOf[SortedIterableFactoryWithBuilder[C]].newBuilder[E]()
    def fromSpecificIterable(from: C[A])(it: Iterable[E]): C[E] = from.sortedIterableFactory.fromSpecificIterable(it)
  }
}

/**
  * Builds a collection of type `C` from elements of type `A`
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait FromSpecificIterable[-A, +C] extends Any with BuildFrom[Any, A, C] {
  def fromSpecificIterable(from: Any)(it: Iterable[A]): C = fromSpecificIterable(it)
  def fromSpecificIterable(it: Iterable[A]): C
}

/** A more specific `FromSpecificIterable` for strict collection types which can provide a `Builder`. */
trait FromSpecificIterableWithBuilder[-A, +C] extends Any with FromSpecificIterable[A, C] with StrictBuildFrom[Any, A, C] {
  def newBuilder(from: Any): Builder[A, C] = newBuilder
  def newBuilder: Builder[A, C]
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+CC[_]] {

  def fromIterable[E](it: Iterable[E]): CC[E]

  def empty[A]: CC[A]

  def apply[A](xs: A*): CC[A] = fromIterable(View.Elems(xs: _*))

  def fill[A](n: Int)(elem: => A): CC[A] = fromIterable(View.Fill(n)(elem))

}

object IterableFactory {
  implicit def toSpecific[A, CC[_]](factory: IterableFactory[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.fromIterable[A](it)
    }

  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty
    def fromIterable[E](it: Iterable[E]): CC[E] = delegate.fromIterable(it)
  }
}

trait IterableFactoryWithBuilder[+CC[_]] extends IterableFactory[CC] {
  def newBuilder[A](): Builder[A, CC[A]]
}

object IterableFactoryWithBuilder {
  implicit def toSpecific[A, CC[_]](factory: IterableFactoryWithBuilder[CC]): FromSpecificIterableWithBuilder[A, CC[A]] =
    new FromSpecificIterableWithBuilder[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.fromIterable[A](it)
      def newBuilder: Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  class Delegate[CC[_]](delegate: IterableFactoryWithBuilder[CC]) extends IterableFactoryWithBuilder[CC] {
    def empty[A]: CC[A] = delegate.empty
    def fromIterable[E](it: Iterable[E]): CC[E] = delegate.fromIterable(it)
    def newBuilder[A](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

trait SpecificIterableFactory[-A, +C] extends FromSpecificIterable[A, C] {
  def empty: C

  def apply(xs: A*): C = fromSpecificIterable(View.Elems(xs: _*))

  def fill(n: Int)(elem: => A): C = fromSpecificIterable(View.Fill(n)(elem))
}

trait SpecificIterableFactoryWithBuilder[-A, +C] extends SpecificIterableFactory[A, C] with FromSpecificIterableWithBuilder[A, C]

/** Factory methods for collections of kind `* −> * -> *` */
trait MapFactory[+CC[X, Y]] {

  def empty[K, V]: CC[K, V]
  def fromIterable[K, V](it: Iterable[(K, V)]): CC[K, V]

  def apply[K, V](elems: (K, V)*): CC[K, V] = fromIterable(elems.toStrawman)
}

object MapFactory {
  implicit def toSpecific[K, V, CC[X, Y]](factory: MapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.fromIterable[K, V](it)
    }

  class Delegate[C[X, Y]](delegate: MapFactory[C]) extends MapFactory[C] {
    def fromIterable[K, V](it: Iterable[(K, V)]): C[K, V] = delegate.fromIterable(it)
    def empty[K, V]: C[K, V] = delegate.empty
  }
}

/** Base trait for companion objects of collections that require an implicit evidence */
trait SortedIterableFactory[+CC[_]] {

  def sortedFromIterable[E : Ordering](it: Iterable[E]): CC[E]

  def empty[A : Ordering]: CC[A]

  def apply[A : Ordering](xs: A*): CC[A] = sortedFromIterable(View.Elems(xs: _*))

  def fill[A : Ordering](n: Int)(elem: => A): CC[A] = sortedFromIterable(View.Fill(n)(elem))
}

object SortedIterableFactory {
  implicit def toSpecific[A: Ordering, CC[_]](factory: SortedIterableFactory[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.sortedFromIterable[A](it)
    }

  class Delegate[CC[_]](delegate: SortedIterableFactory[CC]) extends SortedIterableFactory[CC] {
    def empty[A : Ordering]: CC[A] = delegate.empty
    def sortedFromIterable[E : Ordering](it: Iterable[E]): CC[E] = delegate.sortedFromIterable(it)
  }
}

trait SortedIterableFactoryWithBuilder[+CC[_]] extends SortedIterableFactory[CC] {
  def newBuilder[A : Ordering](): Builder[A, CC[A]]
}

object SortedIterableFactoryWithBuilder {
  implicit def toSpecific[A: Ordering, CC[_]](factory: SortedIterableFactoryWithBuilder[CC]): FromSpecificIterableWithBuilder[A, CC[A]] =
    new FromSpecificIterableWithBuilder[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.sortedFromIterable[A](it)
      def newBuilder: Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  class Delegate[CC[_]](delegate: SortedIterableFactoryWithBuilder[CC]) extends SortedIterableFactoryWithBuilder[CC] {
    def empty[A : Ordering]: CC[A] = delegate.empty
    def sortedFromIterable[E : Ordering](it: Iterable[E]): CC[E] = delegate.sortedFromIterable(it)
    def newBuilder[A : Ordering](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

/** Factory methods for collections of kind `* −> * -> *` which require an implicit evidence value for the key type */
trait SortedMapFactory[+CC[X, Y]] {

  def empty[K : Ordering, V]: CC[K, V]

  def sortedFromIterable[K : Ordering, V](it: Iterable[(K, V)]): CC[K, V]

  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] =
    sortedFromIterable(elems.toStrawman)
}

object SortedMapFactory {
  implicit def toSpecific[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.sortedFromIterable(it)
    }

  class Delegate[CC[_, _]](delegate: SortedMapFactory[CC]) extends SortedMapFactory[CC] {
    def empty[K: Ordering, V]: CC[K, V] = delegate.empty[K, V]
    def sortedFromIterable[K: Ordering, V](it: Iterable[(K, V)]): CC[K, V] = delegate.sortedFromIterable(it)
  }
}
