package structures.list

import scala.reflect.ClassTag

case class KCons[T](override val head: T, override val tail: KList[T]) extends KList[T]:
  val size: Int = 1 + tail.size

  def ::[S >: T](s: S): KList[S]      = KCons(s, this)
  def append[S >: T](s: S): KList[S]  = KCons(head, tail.append(s))
  def add[S >: T](s: S, idx: Int): KList[S] = {
    if (idx < 0) throw new IndexOutOfBoundsException
    else if (idx == 0) s :: this
    else KCons(head, tail.add(s, idx - 1))
  }
  def contains[S >: T](s: S): Boolean = head == s || tail.contains(s)
  def get(idx: Int): Option[T]        =
    if (idx < 0) None
    else if (idx == 0) Some(head)
    else tail.get(idx - 1)
  def indexOf[S >: T](s: S): Int      =
    if (head == s) 0
    else 1 + tail.indexOf(s)

  def map[S](f: T => S): KList[S]                     = KCons(f(head), tail.map(f))
  def filter(f: T => Boolean): KList[T]               =
    if (f(head)) KCons(head, tail.filter(f))
    else tail.filter(f)
  def foldLeft[A, B >: T](zero: A)(f: (A, B) => A): A = tail.foldLeft[A, B](f(zero, head))(f)
  def sum[S >: T](using num: Numeric[S]): S           = foldLeft[S, S](num.zero)(num.plus)
  def prod[S >: T](using num: Numeric[S]): S          = foldLeft[S, S](num.one)(num.times)
  def zip[S](other: KList[S]): KList[(T, S)]          = KCons(head -> other.head, tail zip other.tail)
  def zipWithIndex: KList[(T, Int)]                   = zip(KList(0 until size: _*))
  def foreach(fn: T => Unit): Unit = {
    fn(head)
    tail.foreach(fn)
  }

  def reverse: KList[T] = foldLeft(KNil: KList[T]) { case (l, t) => t :: l }

  def toArray[S >: T: ClassTag]: Array[S] = {
    val result = Array.ofDim[S](size)
    zipWithIndex.foreach { case (s, idx) => result(idx) = s }
    result
  }

  override def toString: String = s"$head :: $tail"
