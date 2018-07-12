package com.spotright.chorus.lib

import scala.collection.mutable

class SList[A]() extends SListLike[A] {

  next = this

  def this(elem: A, next: SList[A]) {
    this()
    if (next != null) {
      this.elem = elem
      this.next = next
    }
  }

  def length: Int = {
    var len = 0
    var crnt = this
    while (crnt.nonEmpty) {
      len += 1
      crnt = crnt.next
    }
    len
  }

  override
  def toString: String = {
    val sb = StringBuilder.newBuilder
    sb ++= "SList("
    var pad = false
    var crnt = this
    while(crnt.nonEmpty) {
      if (pad) sb += ','
      pad = true
      sb ++= crnt.head.toString
      crnt = crnt.next
    }
    sb += ')'
    sb.result()
  }
}

object SList {

  def empty[A]: SList[A] = new SList[A]

  def apply[A](as: A*): SList[A] = {
    val bldr = newBuilder[A]
    as.foreach(bldr += _)
    bldr.result()
  }

  def apply[A](as: Iterator[A]): SList[A] = {
    val bldr = newBuilder[A]
    as.foreach(bldr += _)
    bldr.result()
  }

  def newBuilder[A]: mutable.Builder[A, SList[A]] = new Builder[A]

  class Builder[A] extends mutable.Builder[A, SList[A]] {
    var top = new SList[A]
    var end = top

    def clear(): Unit = {
      top = new SList[A]
      end = top
    }

    def +=(elem: A): this.type = {
      end.elem = elem
      end.next = new SList[A]
      end = end.next
      this
    }

    def result(): SList[A] = top
  }

}
