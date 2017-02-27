package strawman.collection

package object immutable {
  // Allow `.to(SortedSet)`. We can't rename the object `SortedSetFactory` to `SortedSet` because it has to
  // be the companion object of the `SortedSetFactory` trait so that it is eligible for implicit search for
  // `SortedSetFactory#Build`.
  val SortedSet = SortedSetFactory
}
