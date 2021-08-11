package structures.list

import scala.reflect.ClassTag

/** Represents a Linked List of type T. It has been named KList to avoid conflicting with Scala's own List.
 * This is useful also for avoiding conflict with Nil as well.
 *
 * @tparam T
 *   Type of the elements of the list.
 */
trait KList[+T]:
  def head: T
  def tail: KList[T]
  val size: Int

  def ::[S >: T](s: S): KList[S]
  def append[S >: T](s: S): KList[S]
  def add[S >: T](s: S, idx: Int): KList[S]
  def contains[S >: T](s: S): Boolean
  def get(idx: Int): Option[T]
  def indexOf[S >: T](s: S): Int

  def map[S](f: T => S): KList[S]
  def filter(f: T => Boolean): KList[T]
  def foldLeft[A, B >: T](zero: A)(f: (A, B) => A): A
  def sum[S >: T](using num: Numeric[S]): S
  def prod[S >: T](using num: Numeric[S]): S
  def zip[S](other: KList[S]): KList[(T, S)]
  def zipWithIndex: KList[(T, Int)]
  def foreach(fn: T => Unit): Unit

  def reverse: KList[T]

  def toArray[S >: T: ClassTag]: Array[S]

object KList:
  def apply[T](elements: T*): KList[T] =
    if (elements.isEmpty) KNil
    else KCons(elements.head, apply(elements.tail: _*))