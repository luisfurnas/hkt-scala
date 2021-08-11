package structures.list

import scala.reflect.ClassTag

case object KNil extends KList[Nothing]:
  def head: Nothing = throw new NoSuchElementException("head of an empty list")
  def tail: Nothing = throw new NoSuchElementException("tail of an empty list")
  val size: Int     = 0

  def ::[S >: Nothing](s: S): KList[S]            = KCons(s, this)
  def append[S >: Nothing](s: S): KList[S]        = s :: this
  def add[S >: Nothing](s: S, idx: Int): KList[S] =
    if (idx == 0) s :: this
    else throw new IndexOutOfBoundsException
  def contains[S >: Nothing](s: S): Boolean       = false
  def get(idx: Int): Option[Nothing]              = None
  def indexOf[S >: Nothing](s: S): Int            = -1

  def map[S](f: Nothing => S): KList[S]                      = this
  def filter(f: Nothing => Boolean): KList[Nothing]          = this
  def foldLeft[A, Nothing](zero: A)(f: (A, Nothing) => A): A = zero
  def sum[S >: Nothing](using num: Numeric[S]): S            = num.zero
  def prod[S >: Nothing](using num: Numeric[S]): S           = num.one
  def zip[S](other: KList[S]): KList[(Nothing, S)]           = this
  def zipWithIndex: KList[(Nothing, Int)]                    = this
  def foreach(fn: Nothing => Unit): Unit                     = ()

  def reverse: KList[Nothing] = this

  def toArray[S >: Nothing: ClassTag]: Array[S] = Array[S]()

  override def toString: String = "KNil"
