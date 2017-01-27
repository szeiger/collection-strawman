package strawman.collection

import scala.Boolean

/** Partial base trait for set collections
  *
  * Supports only monomorphic transformations.
  */
trait MonoSet[A]
  extends IterableOnce[A]
    with MonoSetLike[A, MonoSet]

trait MonoSetLike[A, +C[X] <: MonoSet[X]]
  extends MonoSetMonoTransforms[A, C[A]] {

  def contains(elem: A): Boolean

}

/** Monomorphic transformation operations */
trait MonoSetMonoTransforms[A, +Repr] {

  def filter(p: A => Boolean): Repr

  /** Intersection of `this` and `that` */
  def & (that: MonoSet[A]): Repr

  /** Union of `this` and `that` */
  def ++ (that: MonoSet[A]): Repr

}

/** Base trait for set collections */
trait Set[A]
  extends MonoSet[A]
    with Iterable[A]
    with SetLike[A, Set]

/** Base trait for set operations */
trait SetLike[A, +C[X] <: Set[X]]
  extends MonoSetLike[A, C]
    with IterableLike[A, C]
