package strawman.collection

import scala.{Any, AnyVal, AnyRef, Nothing, Int}
import scala.annotation.unchecked.uncheckedVariance

trait ConstrainedFromIterable extends ConstrainedFromIterableLowPriority {
  /* The preferred collection type to build for element type `E` */
  type Preferred[E]
  /* The implicit evidence required to build `Preferred[E]` */
  type Constraint[E]
  /* The `Iterable[E]` fallback type to build when no `Constraint[E]` is available */
  type Fallback[E] <: Iterable[E]

  class Build[E, +To]
  class BuildPreferred[E, +To](val constraint: Constraint[E]) extends Build[E, To]

  def constrainedFromIterable[E, To](it: Iterable[E])(implicit ev: Build[E, To]): To

  implicit def buildPreferred[E, To](implicit ev: Constraint[E]): BuildPreferred[E, Preferred[E]] =
    new BuildPreferred[E, Preferred[E]](ev)
}

trait ConstrainedFromIterableLowPriority { this: ConstrainedFromIterable =>
  private[this] val fallback = new Build[Any, Nothing]
  implicit def buildFallback[E, To]: Build[E, Fallback[E]] = fallback.asInstanceOf[Build[E, Fallback[E]]]
}

trait ConstrainedIterableFactory extends ConstrainedFromIterable {
  def empty[X, To](implicit ev: BuildPreferred[X, To]): To = constrainedFromIterable(View.Empty)
  def apply[A, To](xs: A*)(implicit ev: BuildPreferred[A, To]): To = constrainedFromIterable(View.Elems(xs: _*))
  def fill[A, To](n: Int)(elem: => A)(implicit ev: Build[A, To]): To = constrainedFromIterable(View.Fill(n)(elem))
}

trait ConstrainedIterablePolyTransforms[+A, +C[A], +F <: ConstrainedFromIterable] extends IterablePolyTransforms[A, C] {
  protected def coll: Iterable[A]
  def constrainedFromIterable[E, To](coll: Iterable[E])(implicit ev: (F @uncheckedVariance)#Build[E, To]): To

  /** Map */
  def map[B, To](f: A => B)(implicit ev: (F @uncheckedVariance)#Build[B, To]): To = constrainedFromIterable(View.Map(coll, f))

  /** Flatmap */
  def flatMap[B, To](f: A => IterableOnce[B])(implicit ev: (F @uncheckedVariance)#Build[B, To]): To = constrainedFromIterable(View.FlatMap(coll, f))

  /** Concatenation */
  def ++[B >: A, To](xs: IterableOnce[B])(implicit ev: (F @uncheckedVariance)#Build[B, To]): To = constrainedFromIterable(View.Concat(coll, xs))

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B, To](xs: IterableOnce[B])(implicit ev: (F @uncheckedVariance)#Build[(A @uncheckedVariance, B), To]): To = constrainedFromIterable(View.Zip(coll, xs))
  // sound bcs of VarianceNote
}
