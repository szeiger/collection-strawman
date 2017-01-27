package strawman.collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.{Iterator, OrderingGuidedFactories}

import scala.{Boolean, Ordering}
import scala.Predef.???

/** Immutable sorted set backed by a tree */
final class TreeSet[A]()(implicit val ordering: Ordering[A])
  extends SortedSet[A]
    with SortedSetLike[A, TreeSet] {

  // from IterableOnce
  def iterator(): Iterator[A] = ???

  // from MonoSet
  def & (that: strawman.collection.MonoSet[A]): TreeSet[A] = ???
  def ++ (that: strawman.collection.MonoSet[A]): TreeSet[A] = ???
  def contains(elem: A): Boolean = ???

  // from immutable.MonoSet
  def +(elem: A): TreeSet[A] = ???
  def -(elem: A): TreeSet[A] = ???

  // from Sorted
  def range(from: A, until: A): TreeSet[A] = ???

  // from MonoSetMonoTransforms
  def filter(p: A => Boolean): TreeSet[A] = ???

  // from SortedPolyTransforms
  def map[B](f: A => B)(implicit ordering: Ordering[B]): TreeSet[B] = ???

}

object TreeSet extends OrderingGuidedFactories[TreeSet] {

  def builder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = ???

}