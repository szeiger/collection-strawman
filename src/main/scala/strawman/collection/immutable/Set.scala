package strawman.collection.immutable

import strawman.collection.IterableLike

import scala.Any

/** Base trait for immutable set operations supporting only monomorphic transformations */
trait MonoSet[A]
  extends strawman.collection.MonoSet[A]
    with MonoSetLike[A, MonoSet]

trait MonoSetLike[A, +C[X] <: MonoSet[X]]
  extends strawman.collection.MonoSetLike[A, C] {

  def + (elem: A): C[A]

  def - (elem: A): C[A]

}

/** Base trait for immutable set collections */
trait Set[A]
  extends strawman.collection.Set[A]
    with Iterable[A]
    with MonoSet[A]
    with SetLike[A, Set]


/** Base trait for immutable set operations */
trait SetLike[A, +C[X] <: Set[X]]
  extends strawman.collection.SetLike[A, C]