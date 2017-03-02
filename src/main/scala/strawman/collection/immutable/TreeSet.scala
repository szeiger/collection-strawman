package strawman.collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.{ConstrainedIterablePolyTransforms, Iterator, IterableFactory}

import scala.{Boolean, Ordering}
import scala.Predef.???

/** Immutable sorted set backed by a tree */
final class TreeSet[A]()(implicit val ordering: Ordering[A])
  extends SortedSet[A]
    with SortedSetLike[A, TreeSet]
    with ConstrainedIterablePolyTransforms[A, Set, Ordering] {

  type CC[X] = TreeSet[X]

  // From IterableOnce
  def iterator(): Iterator[A] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: strawman.collection.Iterable[B]): Set[B] = ???
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[A]): TreeSet[A] = ???

  // From ConstrainedIterablePolyTransforms
  def constrainedFromIterable[E](coll: strawman.collection.Iterable[E])(implicit ev: Ordering[E]): CC[E] = ???

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

object TreeSet  {
  implicit def factory[B](implicit o: Ordering[B]): IterableFactory[B, TreeSet] = new IterableFactory[B, TreeSet] {
    def fromIterable[E <: B](it: strawman.collection.Iterable[E]): TreeSet[E] = ???
  }
}
