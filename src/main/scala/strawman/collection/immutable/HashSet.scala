package strawman.collection.immutable

import strawman.collection.{IterableFactory, Iterator}

import scala.{Boolean, Any, Int}
import scala.Predef.???

/** An immutable Set backed by a hash trie */
class HashSet[A] extends Set[A] with SetLike[A, HashSet] {

  // From IterableOnce
  def iterator(): Iterator[A] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: strawman.collection.Iterable[B]): HashSet[B] = ???

  // From IterableMonoTransforms
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[A]): HashSet[A] = fromIterable(coll)

  // From SetLike
  def contains(elem: A): Boolean = ???
  def subsetOf(that: strawman.collection.Set[A]): Boolean = ???

  // From SetMonoTransforms
  def & (that: strawman.collection.Set[A]): HashSet[A] = ???
  def ++ (that: strawman.collection.Set[A]): HashSet[A] = ???

  // From immutable.SetLike
  def + (elem: A): HashSet[A] = ???
  def - (elem: A): HashSet[A] = ???

}

object HashSet extends IterableFactory[Any, HashSet] {
  def fromIterable[B](it: strawman.collection.Iterable[B]): HashSet[B] = ???
}
