package strawman.collection.immutable

import strawman.collection.{IterableFactory, Iterator}

import scala.{Boolean, Any, Int}
import scala.Predef.???

class BitSet extends Set[Int] with SetLike[Int, Set] with SetMonoTransforms[Int, BitSet] {

  // From IterableOnce
  def iterator(): Iterator[Int] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: strawman.collection.Iterable[B]): Set[B] = ???

  // From IterableMonoTransforms
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[Int]): BitSet = ???

  // From SetLike
  def contains(elem: Int): Boolean = ???
  def subsetOf(that: strawman.collection.Set[Int]): Boolean = ???

  // From SetMonoTransforms
  def & (that: strawman.collection.Set[Int]): BitSet = ???
  def ++ (that: strawman.collection.Set[Int]): BitSet = ???

  // From immutable.SetLike
  def + (elem: Int): BitSet = ???
  def - (elem: Int): BitSet = ???
}

object BitSet extends IterableFactory[Int, BitSetHelper.BitSetCons] {
  def fromIterable[E <: Int](it: strawman.collection.Iterable[E]): BitSetHelper.BitSetCons[E] = ???
}

private[collection] object BitSetHelper {
  // A unary type constructor for BitSet
  type BitSetCons[X] = BitSet with strawman.collection.Iterable[X]
}
