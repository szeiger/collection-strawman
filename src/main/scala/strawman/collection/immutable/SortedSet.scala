package strawman.collection.immutable

import scala.Ordering
import scala.Predef.???
import strawman.collection.mutable.Builder
import strawman.collection.{ConstrainedIterableFactory, ConstrainedIterablePolyTransforms, Sorted, SortedLike}

/** Base trait for sorted sets */
trait SortedSet[A]
  extends Set[A]
    with Sorted[A]
    with SortedSetLike[A, SortedSet] // Inherited SortedSet operations return a `SortedSet`

trait SortedSetLike[A, +C[X] <: SortedSet[X]]
  extends SortedLike[A, C[A]]
    with ConstrainedIterablePolyTransforms[A, Set, SortedSetFactory]
    with SetLike[A, Set] // Inherited Set operations return a `Set`
    with SetMonoTransforms[A, C[A]] // Override the return type of Set ops to return C[A]

trait SortedSetFactory extends ConstrainedIterableFactory {
  type Fallback[E] = Set[E]
  type Preferred[E] <: SortedSet[E]
  type Constraint[E] = Ordering[E]
}

object SortedSetFactory extends SortedSetFactory {
  type Preferred[E] = SortedSet[E]

  def builder[A](implicit ordering: Ordering[A]): Builder[A, SortedSet[A]] = ???

  def constrainedFromIterable[E, To](it: strawman.collection.Iterable[E])(implicit ev: Build[E, To]): To = ???
}
