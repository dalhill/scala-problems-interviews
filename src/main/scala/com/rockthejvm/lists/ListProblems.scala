package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def prepend[S >: T](elem: S): RList[S] = Cons(elem, this)

  def apply(index: Int): T
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException
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
}

object ListProblems extends App {
  val test = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, RNil)))))
  println(test(4))
}
