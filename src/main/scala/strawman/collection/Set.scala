package strawman
package collection

import scala.Boolean

/** Base trait for set collections */
trait Set[A]
  extends Iterable[A]
    with SetLike[A, Set]

/** Base trait for set operations */
trait SetLike[A, +C[X] <: Set[X]]
  extends IterableLike[A, C]
    with SetMonoTransforms[A, C[A]] {

  def contains(elem: A): Boolean

}

/** Monomorphic transformation operations */
trait SetMonoTransforms[A, +Repr] {

  def & (that: Set[A]): Repr

  def ++ (that: Set[A]): Repr

}