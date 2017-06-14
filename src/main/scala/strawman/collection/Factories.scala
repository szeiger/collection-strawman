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

object BuildFrom extends BuildFromLowPriority {
  /** Build the source collection type from a MapOps */
  implicit def buildFromMapOps[CC[K, V] <: Map[K, V] with MapOps[K, V, CC, _], A, B, E, F]: BuildFrom[CC[A, B], (E, F), CC[E, F]] = new BuildFrom[CC[A, B], (E, F), CC[E, F]] {
    //TODO: Reuse a prototype instance
    def fromSpecificIterable(from: CC[A, B])(it: Iterable[(E, F)]): CC[E, F] = from.mapFactory.fromIterable(it)
  }

  /** Build the source collection type from a SortedMapOps */
  implicit def buildFromSortedMapOps[CC[K, V] <: SortedMap[K, V] with SortedMapOps[K, V, CC, _], A, B, E : Ordering, F]: BuildFrom[CC[A, B], (E, F), CC[E, F]] = new BuildFrom[CC[A, B], (E, F), CC[E, F]] {
    //TODO: Reuse a prototype instance
    def fromSpecificIterable(from: CC[A, B])(it: Iterable[(E, F)]): CC[E, F] = from.sortedMapFactory.fromSpecificIterable(it)
  }
}

trait BuildFromLowPriority {
  /** Build the source collection type from an IterableOps */
  implicit def buildFromIterableOps[CC[X] <: IterableOps[X, CC, _], A, E]: BuildFrom[CC[A], E, CC[E]] = new BuildFrom[CC[A], E, CC[E]] {
    //TODO: Reuse a prototype instance
    def fromSpecificIterable(from: CC[A])(it: Iterable[E]): CC[E] = from.iterableFactory.fromIterable(it)
  }

  /** Build the source collection type from an Iterable with SortedOps */
  implicit def buildFromSortedOps[CC[X] <: Iterable[X] with SortedOps[X, CC[X], CC], A, E : Ordering]: BuildFrom[CC[A], E, CC[E]] = new BuildFrom[CC[A], E, CC[E]] {
    def fromSpecificIterable(from: CC[A])(it: Iterable[E]): CC[E] = from.sortedIterableFactory.fromSpecificIterable(it)
  }
}

/** A more specific `BuildFrom` for strict target collection types which can provide a `Builder`.
  * Note that a `Builder` can be obtained for any `BuildFrom` via `Builder.from`.
  */
trait StrictBuildFrom[-From, -A, +C] extends Any with BuildFrom[From, A, C] {
  def newBuilder(from: From): Builder[A, C]
}

object StrictBuildFrom extends StrictBuildFromLowPriority {
  /** Build the source collection type from a strict MapOps */
  implicit def strictBuildFromMapOps[CC[K, V] <: Map[K, V] with MapOps[K, V, CC, _], A, B, E, F]: StrictBuildFrom[CC[A, B], (E, F), CC[E, F]] = new StrictBuildFrom[CC[A, B], (E, F), CC[E, F]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A, B]): Builder[(E, F), CC[E, F]] = from.mapFactory.asInstanceOf[MapFactoryWithBuilder[CC]].newBuilder[E, F]()
    def fromSpecificIterable(from: CC[A, B])(it: Iterable[(E, F)]): CC[E, F] = from.mapFactory.fromIterable(it)
  }

  /** Build the source collection type from a strict SortedMapOps */
  implicit def strictBuildFromSortedMapOps[CC[K, V] <: SortedMap[K, V] with SortedMapOps[K, V, CC, _], A, B, E : Ordering, F]: StrictBuildFrom[CC[A, B], (E, F), CC[E, F]] = new StrictBuildFrom[CC[A, B], (E, F), CC[E, F]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A, B]): Builder[(E, F), CC[E, F]] = from.sortedMapFactory.asInstanceOf[SortedMapFactoryWithBuilder[CC]].newBuilder[E, F]()
    def fromSpecificIterable(from: CC[A, B])(it: Iterable[(E, F)]): CC[E, F] = from.sortedMapFactory.fromSpecificIterable(it)
  }
}

trait StrictBuildFromLowPriority {
  /** Build the source collection type from a strict IterableOps */
  implicit def strictBuildFromIterableOps[CC[X] <: IterableOps[X, CC, _] with Buildable[X, CC[X]], A, E]: StrictBuildFrom[CC[A], E, CC[E]] = new StrictBuildFrom[CC[A], E, CC[E]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A]): Builder[E, CC[E]] = from.iterableFactory.asInstanceOf[IterableFactoryWithBuilder[CC]].newBuilder[E]()
    def fromSpecificIterable(from: CC[A])(it: Iterable[E]): CC[E] = from.iterableFactory.fromIterable(it)
  }

  /** Build the source collection type from a strict Iterable with SortedOps */
  implicit def strictBuildFromSortedOps[CC[X] <: Iterable[X] with SortedOps[X, CC[X], CC] with Buildable[X, CC[X]], A, E : Ordering]: StrictBuildFrom[CC[A], E, CC[E]] = new StrictBuildFrom[CC[A], E, CC[E]] {
    def newBuilder(from: CC[A]): Builder[E, CC[E]] = from.sortedIterableFactory.asInstanceOf[SortedIterableFactoryWithBuilder[CC]].newBuilder[E]()
    def fromSpecificIterable(from: CC[A])(it: Iterable[E]): CC[E] = from.sortedIterableFactory.fromSpecificIterable(it)
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

trait MapFactoryWithBuilder[+CC[X, Y]] extends MapFactory[CC] {
  def newBuilder[K, V](): Builder[(K, V), CC[K, V]]
}

object MapFactoryWithBuilder {
  implicit def toSpecific[K, V, CC[X, Y]](factory: MapFactoryWithBuilder[CC]): FromSpecificIterableWithBuilder[(K, V), CC[K, V]] =
    new FromSpecificIterableWithBuilder[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.fromIterable[K, V](it)
      def newBuilder: Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]()
    }

  class Delegate[C[X, Y]](delegate: MapFactoryWithBuilder[C]) extends MapFactoryWithBuilder[C] {
    def fromIterable[K, V](it: Iterable[(K, V)]): C[K, V] = delegate.fromIterable(it)
    def empty[K, V]: C[K, V] = delegate.empty
    def newBuilder[K, V](): Builder[(K, V), C[K, V]] = delegate.newBuilder()
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

trait SortedMapFactoryWithBuilder[+CC[X, Y]] extends SortedMapFactory[CC] {
  def newBuilder[K : Ordering, V](): Builder[(K, V), CC[K, V]]
}

object SortedMapFactoryWithBuilder {
  implicit def toSpecific[K : Ordering, V, CC[X, Y]](factory: SortedMapFactoryWithBuilder[CC]): FromSpecificIterableWithBuilder[(K, V), CC[K, V]] =
    new FromSpecificIterableWithBuilder[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.fromSpecificIterable(it)
      def newBuilder: Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]()
    }

  class Delegate[C[X, Y]](delegate: SortedMapFactoryWithBuilder[C]) extends SortedMapFactoryWithBuilder[C] {
    def sortedFromIterable[K : Ordering, V](it: Iterable[(K, V)]): C[K, V] = delegate.sortedFromIterable(it)
    def empty[K : Ordering, V]: C[K, V] = delegate.empty
    def newBuilder[K : Ordering, V](): Builder[(K, V), C[K, V]] = delegate.newBuilder()
  }
}
