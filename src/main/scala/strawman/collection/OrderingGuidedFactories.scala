package strawman.collection

import strawman.collection.mutable.Builder

import scala.Ordering

/**
  * Factories for collections whose elements require an ordering
  */
trait OrderingGuidedFactories[C[_]] {

  def builder[A](implicit ordering: Ordering[A]): Builder[A, C[A]]

  def empty[A](implicit ordering: Ordering[A]): C[A] =
    builder[A].result

  def apply[A](as: A*)(implicit ordering: Ordering[A]): C[A] =
    (builder[A] ++= as.toStrawman).result

}
