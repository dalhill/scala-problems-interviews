package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def prepend[S >: T](elem: S): RList[S] = Cons(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this
}

case class Cons[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  override def apply(index: Int): T = {
    @tailrec
    def tailrecApply(rl: RList[T], i: Int=0): T = {
      if (i == index) rl.head
      else tailrecApply(rl.tail, i+1)
    }

    if (index < 0) throw new NoSuchElementException
    else tailrecApply(this)
  }

  override def length: Int = {
    @tailrec
    def helper(rl: RList[T], i: Int=0): Int = {
      if (rl.isEmpty) i
      else helper(rl.tail, i+1)
    }
    helper(this)
  }

  override def reverse: RList[T] = {
    @tailrec
    def helper(rl: RList[T], newList: RList[T]): RList[T] = {
      if (rl.isEmpty) newList
      else helper(rl.tail, newList.prepend(rl.head))
    }

    helper(this, RNil)
  }
}


object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def helper(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else helper(remaining.tail, acc.prepend(remaining.head))
    }

    helper(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  val largeList = RList.from(1 to 25)
  println(largeList(4))
  println(largeList.length)
  println(largeList.reverse)
}
