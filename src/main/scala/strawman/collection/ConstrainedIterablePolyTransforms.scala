package strawman.collection

import scala.{Any, AnyVal, AnyRef, Nothing, Int}
import scala.annotation.unchecked.uncheckedVariance
import scala.Predef.???

trait ConstrainedIterablePolyTransforms[+A, +C[A], Ev[A]] extends IterablePolyTransforms[A, C] {
  type CC[X] <: C[X]

  protected def coll: Iterable[A]

  def constrainedFromIterable[E](coll: Iterable[E])(implicit ev: Ev[E]): CC[E]

  /** Map */
  def map[B](f: A => B)(implicit ev: Ev[B]): CC[B] = constrainedFromIterable(View.Map(coll, f))

  /** Flatmap */
  def flatMap[B](f: A => IterableOnce[B])(implicit ev: Ev[B]): CC[B] = constrainedFromIterable(View.FlatMap(coll, f))

  /** Concatenation */
  def ++[B >: A](xs: IterableOnce[B])(implicit ev: Ev[B]): CC[B] = constrainedFromIterable(View.Concat(coll, xs))

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B](xs: IterableOnce[B])(implicit ev: Ev[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = constrainedFromIterable(View.Zip(coll, xs))
  // sound bcs of VarianceNote
}
