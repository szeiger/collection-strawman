package strawman.collection

import scala.{Ordering}

/** Base trait for sorted collections */
trait Sorted[A]
  extends SortedLike[A, Sorted]

trait SortedLike[A, +C[X] <: Sorted[X]] {

  def ordering: Ordering[A]

  /** Ranged projection of this collection with both a lower-bound and an upper-bound */
  def range(from: A, until: A): C[A]

}

/** Polymorphic transformation methods on sorted collections.
  */
trait SortedPolyTransforms[A, +C[X] <: Sorted[X]] {

  def map[B](f: A => B)(implicit ordering: Ordering[B]): C[B]

}