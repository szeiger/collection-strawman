package strawman.collection.immutable

/** Immutable `Seq` */
trait Seq[+A] extends Iterable[A] with strawman.collection.Seq[A] with strawman.collection.SeqLike[A, Seq]

