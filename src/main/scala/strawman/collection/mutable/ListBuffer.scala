package strawman.collection.mutable

import scala.{Int, Unit, Boolean, Any}
import scala.Int._
import strawman.collection
import strawman.collection.{Iterator, IterableOnce, IterableFactory, SeqLike}
import strawman.collection.immutable.{List, Nil, ::}
import scala.annotation.tailrec
import java.lang.IndexOutOfBoundsException
import scala.Predef.{assert, intWrapper}

/** Concrete collection type: ListBuffer */
class ListBuffer[A]
  extends Seq[A]
    with SeqLike[A, ListBuffer]
    with Buildable[A, ListBuffer[A]]
    with Builder[A, ListBuffer[A]] {

  private var first: List[A] = Nil
  private var last: ::[A] = null
  private var aliased = false
  private var len = 0

  private type Predecessor[A] = ::[A] /*| Null*/

  def iterator() = first.iterator()

  def fromIterable[B](coll: collection.Iterable[B]) = ListBuffer.fromIterable(coll)

  def apply(i: Int) = first.apply(i)

  def length = len
  override def knownSize = len

  protected[this] def newBuilder = new ListBuffer[A]

  private def copyElems(): Unit = {
    val buf = ListBuffer.fromIterable(result)
    first = buf.first
    last = buf.last
    aliased = false
  }

  private def ensureUnaliased() = if (aliased) copyElems()

  /** Convert to list; avoids copying where possible. */
  def toList = {
    aliased = true
    first
  }

  def clear(): Unit = {
    first = Nil
  }

  def +=(elem: A) = {
    ensureUnaliased()
    val last1 = (elem :: Nil).asInstanceOf[::[A]]
    if (len == 0) first = last1 else last.next = last1
    last = last1
    len += 1
    this
  }

  private def locate(i: Int): Predecessor[A] =
    if (i == 0) null
    else if (i == len) last
    else {
      var j = i - 1
      var p = first
      while (j > 0) {
        p = p.tail
        j -= 1
      }
      p.asInstanceOf[Predecessor[A]]
    }

  private def getNext(p: Predecessor[A]): List[A] =
    if (p == null) first else p.next

  private def setNext(p: Predecessor[A], nx: List[A]): Unit =
    if (p == null) first = nx else p.next = nx

  def update(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw new IndexOutOfBoundsException
    val p = locate(idx)
    setNext(p, elem :: getNext(p).tail)
  }

  def insert(idx: Int, elem: A): Unit = {
    ensureUnaliased()
    if (idx < 0 || idx > len) throw new IndexOutOfBoundsException
    if (idx == len) +=(elem)
    else {
      val p = locate(idx)
      setNext(p, elem :: getNext(p))
      len += 1
    }
  }

  private def insertAfter(p: Predecessor[A], it: Iterator[A]) = {
    var prev = p
    val follow = getNext(prev)
    while (it.hasNext) {
      len += 1
      val next = (it.next :: follow).asInstanceOf[::[A]]
      setNext(prev, next)
      prev = next
    }
  }

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    ensureUnaliased()
    val it = elems.iterator()
    if (it.hasNext) {
      ensureUnaliased()
      if (idx < 0 || idx > len) throw new IndexOutOfBoundsException
      if (idx == len) ++=(elems)
      else insertAfter(locate(idx), it)
    }
  }

  def remove(idx: Int): A = {
    ensureUnaliased()
    if (idx < 0 || idx >= len) throw new IndexOutOfBoundsException
    len -= 1
    val p = locate(idx)
    val nx = getNext(p)
    setNext(p, nx.tail)
    nx.head
  }

  def remove(idx: Int, n: Int): Unit =
    if (n > 0) {
      ensureUnaliased()
      if (idx < 0 || idx + n > len) throw new IndexOutOfBoundsException
      removeAfter(locate(idx), n)
    }

  private def removeAfter(prev: Predecessor[A], n: Int) = {
    @tailrec def ahead(p: List[A], n: Int): List[A] =
      if (n == 0) p else ahead(p.tail, n - 1)
    setNext(prev, ahead(getNext(prev), n))
    len -= n
  }

  def mapInPlace(f: A => A): this.type = {
    ensureUnaliased()
    val buf = new ListBuffer[A]
    for (elem <- this) buf += f(elem)
    first = buf.first
    last = buf.last
    this
  }

  def flatMapInPlace(f: A => IterableOnce[A]): this.type = {
    ensureUnaliased()
    val prev: Predecessor[A] = null
    var cur: List[A] = first
    while (!cur.isEmpty) {
      val follow = cur.tail
      setNext(prev, follow)
      len -= 1
      insertAfter(prev, f(cur.head).iterator())
      cur = follow
    }
    this
  }

  def filterInPlace(p: A => Boolean): this.type = {
    ensureUnaliased()
    var prev: Predecessor[A] = null
    var cur: List[A] = first
    while (!cur.isEmpty) {
      val follow = cur.tail
      if (!p(cur.head)) {
        setNext(prev, follow)
        len -= 1
      }
      prev = cur.asInstanceOf[Predecessor[A]]
      cur = follow
    }
    this
  }

  def patchInPlace(from: Int, patch: collection.Seq[A], replaced: Int): this.type = {
    ensureUnaliased()
    val p = locate(from)
    removeAfter(p, replaced `min` (length - from))
    insertAfter(p, patch.iterator())
    this
  }

  def result = this

  override def className = "ListBuffer"
}

object ListBuffer extends IterableFactory[Any, ListBuffer] {
  def fromIterable[B](coll: collection.Iterable[B]): ListBuffer[B] = new ListBuffer[B] ++= coll
}
