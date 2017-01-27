package strawman.collection.immutable

import strawman.collection.{Sorted, SortedLike, SortedPolyTransforms}

/** Base trait for immutable sets whose values have an ordering */
trait SortedSet[A]
  extends Sorted[A]
    with MonoSet[A]
    with SortedSetLike[A, SortedSet]

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends SortedLike[A, C]
    with MonoSetLike[A, C]
    with SortedPolyTransforms[A, C]