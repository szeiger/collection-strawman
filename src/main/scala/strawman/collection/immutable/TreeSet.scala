package strawman.collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.{ConstrainedIterableFactory, ConstrainedIterablePolyTransforms, Iterator}

import scala.{Boolean, Ordering}
import scala.Predef.???

/** Immutable sorted set backed by a tree */
final class TreeSet[A]()(implicit val ordering: Ordering[A])
  extends SortedSet[A]
    with SortedSetLike[A, TreeSet]
    with ConstrainedIterablePolyTransforms[A, Set, TreeSet.type] {

  // From IterableOnce
  def iterator(): Iterator[A] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: strawman.collection.Iterable[B]): Set[B] = ???
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[A]): TreeSet[A] = TreeSet.builder[A].++=(coll).result

  // From ConstrainedIterablePolyTransforms
  def constrainedFromIterable[E, To](coll: strawman.collection.Iterable[E])(implicit ev: TreeSet.type#Build[E, To]): To = ???

  // From SetLike
  def contains(elem: A): Boolean = ???
  def subsetOf(that: strawman.collection.Set[A]): Boolean = ???

  // From SetMonoTransforms
  def & (that: strawman.collection.Set[A]): TreeSet[A] = ???
  def ++ (that: strawman.collection.Set[A]): TreeSet[A] = ???

  // From immutable.SetMonoTransforms
  def +(elem: A): TreeSet[A] = ???
  def -(elem: A): TreeSet[A] = ???

  // From SortedLike
  def range(from: A, until: A): TreeSet[A] = ???
}

object TreeSet extends SortedSetFactory {
  type Preferred[E] = TreeSet[E]

  def builder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = ???

  def constrainedFromIterable[E, To](it: strawman.collection.Iterable[E])(implicit ev: Build[E, To]): To = ???
}
