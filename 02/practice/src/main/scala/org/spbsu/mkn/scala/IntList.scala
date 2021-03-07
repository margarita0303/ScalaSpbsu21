package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList

  def foldLeft(f: Int => Int => Int)(init: Int): Int
}

object IntList {
  def fromSeq(seq: Seq[Int]): IntList = seq match {
    case Nil => IntNil
    case _ => seq.head :: fromSeq(seq.tail)
  }

  def sum(intList: IntList): Int = intList match {
    case IntNil => undef
    case _ => intList.foldLeft(x => x + _)(0)
  }

  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def size(intList: IntList): Int = intList.foldLeft(_ => _ + 1)(0)
}

case object IntNil extends IntList {
  override def head: Int = undef

  override def tail: IntList = undef

  override def drop(n: Int): IntList = if (n == 0) this else undef

  override def take(n: Int): IntList = if (n == 0) this else undef

  override def map(f: Int => Int): IntList = this

  override def ::(elem: Int): IntList = IntCons(elem, IntNil)

  override def foldLeft(f: Int => Int => Int)(init: Int): Int = init
}

case class IntCons(override val head: Int, override val tail: IntList) extends IntList {
  override def drop(n: Int): IntList = n match {
    case i if i < 0 => IntNil
    case 0 => this
    case _ => tail drop (n - 1)
  }

  override def take(n: Int): IntList = n match {
    case i if i <= 0 => IntNil
    case _ => head :: (tail take (n - 1))
  }

  override def map(f: Int => Int): IntList = f(head) :: (tail map f)

  override def ::(elem: Int): IntList = IntCons(elem, head :: tail)

  override def foldLeft(f: Int => Int => Int)(init: Int): Int = tail.foldLeft(f)(f(head)(init))
}
