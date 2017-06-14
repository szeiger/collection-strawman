package strawman

package collection.test

import org.junit.Test
import strawman.collection._
import strawman.collection.mutable.{ArrayBuffer, Builder, Growable}

import scala.{Any, Either, Int, Left, None, Option, Right, Some, Unit, PartialFunction, Boolean}
import java.lang.String
import scala.Predef.ArrowAssoc
import scala.math.Ordering

class TraverseTest {

  // You can either overload methods for IterableOps and Iterable with SortedOps (if you want to support constrained collection types)
  def optionSequence1[C[X] <: IterableOps[X, C, _], A](xs: C[Option[A]]): Option[C[A]] =
    xs.foldLeft[Option[Builder[A, C[A]]]](Some(Builder.from[A, C](xs.iterableFactory))) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)
  def optionSequence1[C[X] <: Iterable[X] with SortedOps[X, C[X], C], A : Ordering](xs: C[Option[A]]): Option[C[A]] =
    xs.foldLeft[Option[Builder[A, C[A]]]](Some(Builder.from[A, C](xs.sortedIterableFactory))) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  // ...or use BuildFrom to abstract over both and also allow building arbitrary collection types
  def optionSequence2[CC[X] <: Iterable[X], A, To](xs: CC[Option[A]])(implicit bf: BuildFrom[CC[Option[A]], A, To]): Option[To] =
    xs.foldLeft[Option[Builder[A, To]]](Some(Builder.from(bf, xs))) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  // Using dependent types:
  def optionSequence3[A, To](xs: Iterable[Option[A]])(implicit bf: BuildFrom[xs.type, A, To]): Option[To] =
    xs.foldLeft[Option[Builder[A, To]]](Some(Builder.from[xs.type, A, To](bf, xs))) {
      case (Some(builder), Some(a)) => Some(builder += a)
      case _ => None
    }.map(_.result)

  def eitherSequence[A, B, To](xs: Iterable[Either[A, B]])(implicit bf: BuildFrom[xs.type, B, To]): Either[A, To] =
    xs.foldLeft[Either[A, Builder[B, To]]](Right(Builder.from[xs.type, B, To](bf, xs))) {
      case (Right(builder), Right(b)) => Right(builder += b)
      case (Left(a)       ,        _) => Left(a)
      case (_             ,  Left(a)) => Left(a)
    }.map(_.result)

  @Test
  def optionSequence1Test: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence1(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2: immutable.TreeSet[Option[String]] = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence1(xs2)
    val o2t: Option[immutable.Set[String]] = o2

    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence1(xs4)
    val o4t: Option[immutable.List[(Int, String)]] = o4
    val o5: Option[immutable.TreeMap[Int, String]] = o4.map(_.to(immutable.TreeMap))
  }

  @Test
  def optionSequence2Test: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence2(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence2(xs2)
    val o2t: Option[immutable.TreeSet[String]] = o2

    // Breakout-like use case from https://github.com/scala/scala/pull/5233:
    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence2(xs4)(immutable.TreeMap) // same syntax as in `.to`
    val o4t: Option[immutable.TreeMap[Int, String]] = o4
  }

  @Test
  def optionSequence3Test: Unit = {
    val xs1 = immutable.List(Some(1), None, Some(2))
    val o1 = optionSequence3(xs1)
    val o1t: Option[immutable.List[Int]] = o1

    val xs2 = immutable.TreeSet(Some("foo"), Some("bar"), None)
    val o2 = optionSequence3(xs2)
    val o2t: Option[immutable.TreeSet[String]] = o2

    // Breakout-like use case from https://github.com/scala/scala/pull/5233:
    val xs4 = immutable.List[Option[(Int, String)]](Some((1 -> "a")), Some((2 -> "b")))
    val o4 = optionSequence3(xs4)(immutable.TreeMap) // same syntax as in `.to`
    val o4t: Option[immutable.TreeMap[Int, String]] = o4
  }

  @Test
  def eitherSequenceTest: Unit = {
    val xs3 = mutable.ListBuffer(Right("foo"), Left(0), Right("bar"))
    val e1 = eitherSequence(xs3)
    val e1t: Either[Int, mutable.ListBuffer[String]] = e1
  }

  // From https://github.com/scala/collection-strawman/issues/44
  def flatCollect[A, B, To](coll: Iterable[A])(f: PartialFunction[A, IterableOnce[B]])
                       (implicit bf: BuildFrom[coll.type, B, To]): To = {
    val builder = Builder.from[coll.type, B, To](bf, coll)
    for (a <- coll) {
      if (f.isDefinedAt(a)) builder ++= f(a)
    }
    builder.result
  }

  def mapSplit[A, B, C, ToL, ToR](coll: Iterable[A])(f: A => Either[B, C])
              (implicit bfLeft:  BuildFrom[coll.type, B, ToL], bfRight: BuildFrom[coll.type, C, ToR]): (ToL, ToR) = {
    val left = Builder.from[coll.type, B, ToL](bfLeft, coll)
    val right = Builder.from[coll.type, C, ToR](bfRight, coll)
    for (a <- coll)
      f(a).fold(left.add, right.add)
    (left.result, right.result)
  }


  @Test
  def flatCollectTest: Unit = {
    val xs1 = immutable.List(1, 2, 3)
    val xs2 = flatCollect(xs1) { case 2 => mutable.ArrayBuffer("foo", "bar") }
    val xs3: immutable.List[String] = xs2

    val xs4 = immutable.TreeMap((1, "1"), (2, "2"))
    val xs5 = flatCollect(xs4) { case (2, v) => immutable.List((v, v)) }
    val xs6: immutable.TreeMap[String, String] = xs5

    val xs7 = immutable.HashMap((1, "1"), (2, "2"))
    val xs8 = flatCollect(xs7) { case (2, v) => immutable.List((v, v)) }
    val xs9: immutable.HashMap[String, String] = xs8
  }

  @Test
  def mapSplitTest: Unit = {
    val xs1 = immutable.List(1, 2, 3)
    val (xs2, xs3) = mapSplit(xs1)(x => if (x % 2 == 0) Left(x) else Right(x.toString))
    val xs4: immutable.List[Int] = xs2
    val xs5: immutable.List[String] = xs3

    val xs6 = immutable.TreeMap((1, "1"), (2, "2"))
    val (xs7, xs8) = mapSplit(xs6) { case (k, v) => Left[(String, Int), (Int, Boolean)]((v, k)) }
    val xs9: immutable.TreeMap[String, Int] = xs7
    val xs10: immutable.TreeMap[Int, Boolean] = xs8
  }
}
