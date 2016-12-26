package strawman.collection.immutable

import scala.{Any, Boolean, Int}
import scala.util.hashing.MurmurHash3

/**
  * Base class of immutable collections. (Must *not* be inherited by mutable collections)
  */
trait Iterable[+A]
  extends strawman.collection.Iterable[A]
  with strawman.collection.IterableLike[A, Iterable] {

  override def equals(o: Any): Boolean = {
    o match {
      case iterable: Iterable[A] => sameElements(iterable)
      case _ => false
    }
  }

  override def hashCode(): Int =
    IterableUtils.orderedHash(this, IterableUtils.iterableSeed)

}

// Temporary: TODO move to MurmurHash3.scala
object IterableUtils {

  val iterableSeed: Int = "Iterable".##

  final def orderedHash(xs: Iterable[_], seed: Int): Int = {
    var n = 0
    var h = seed
    xs foreach { x =>
      h = MurmurHash3.mix(h, x.##)
      n += 1
    }
    MurmurHash3.finalizeHash(h, n)
  }

}