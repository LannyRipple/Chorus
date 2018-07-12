package com.spotright.chorus.lib

trait SListLike[A] {
  var elem: A = _
  var next: SList[A] = _

  def isEmpty: Boolean = next eq this
  def nonEmpty: Boolean = !isEmpty

  def get: Option[A] = Option(elem)

  def head: A = if (!isEmpty) elem else throw new NoSuchElementException

  def tail: SList[A] = {
    require(nonEmpty, "tail of empty list")
    next
  }
}

