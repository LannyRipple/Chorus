package com.spotright.common.lib

/*
 * Copyright 2011 Spot Influence, LLC.
 *
 * This program is only provided under the terms of a written license from Spot Influence, LLC.
 * Except where otherwise stated in the license this program is provided WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

import scala.collection.IterableLike
import scala.collection.mutable
import scala.collection.generic.{CanBuildFrom, Growable, Shrinkable}

protected[lib] sealed abstract
class PHNode[+A] extends Serializable with Traversable[A] with mutable.Cloneable[PHNode[A]] {

  def foreach[U](f: A => U) {}

  def isDefined: Boolean
}

protected[lib]
case class LeafNode[A](item: A, elts: List[PHNode[A]]) extends PHNode[A] {

  override
  def clone(): PHNode[A] = {
    val newElts = List.newBuilder[PHNode[A]]
    newElts ++= elts
    LeafNode(item, newElts.result())
  }

  override
  def foreach[U](f: A => U) {
    f(item)
    for (e <- elts) e.foreach(f)
  }

  def isDefined: Boolean = true

  override
  def isEmpty: Boolean = false
}

protected[lib] case
object EmptyNode extends PHNode[Nothing] {

  def isDefined: Boolean = false

  override
  def isEmpty: Boolean = true
}

protected[lib]
object PHNode {

  def contains[A](node: PHNode[A], that: A)(implicit ord: Ordering[A]): Boolean = node match {
    case EmptyNode => false
    case LeafNode(item, elts) => ord.compare(item, that) match {
      case 0 => true
      case _ => elts.exists {contains(_, that)}
    }
  }

  def merge[A](a: PHNode[A], b: PHNode[A])(implicit ord: Ordering[A]): PHNode[A] = {
    if (a.isEmpty) b
    else if (b.isEmpty) a
    else {
      val nodeA = a.asInstanceOf[LeafNode[A]]
      val nodeB = b.asInstanceOf[LeafNode[A]]
      val LeafNode(itemA, eltsA) = nodeA
      val LeafNode(itemB, eltsB) = nodeB

      // prefer B on == to maintain stability
      if (ord.compare(itemA, itemB) < 0) LeafNode(itemA, nodeB :: eltsA)
      else LeafNode(itemB, nodeA :: eltsB)
    }
  }

  def mergePairs[A](xs: List[PHNode[A]])(implicit ord: Ordering[A]): PHNode[A] = xs match {
    case Nil => EmptyNode
    case List(node) => node
    case head :: next :: rest => merge(merge(head, next), mergePairs(rest))
  }

  /**
   * remove first occurance of a node from the heap
   *
   * @tparam A node type
   * @param node the pairingheap node to act on
   * @param that the item to try and remove
   */
  def remove[A](node: PHNode[A], that: A)(implicit ord: Ordering[A]): (PHNode[A], Option[A]) = {
    def eltsRemove(z: List[PHNode[A]], elts: List[PHNode[A]]): (List[PHNode[A]], Option[A]) = elts match {
      case Nil => (z, None)
      case a :: rest =>
        val (node, maybeRv) = remove(a, that)

        if (maybeRv.isEmpty) eltsRemove(a :: z, rest)
        else node match {
          case EmptyNode => (z.foldLeft(rest) {(z, a) => a :: z}, maybeRv)
          case node => (z.foldLeft(node :: rest) {(z, a) => a :: z}, maybeRv)
        }
    }

    node match {
      case EmptyNode => (node, None)
      case LeafNode(item, elts) => ord.compare(that, item) match {
        case -1 => (node, None)
        case 0 => (mergePairs(elts), Some(item))
        case _ =>
          val (newElts, maybeFound) = eltsRemove(Nil, elts)

          if (maybeFound.isEmpty) (node, None)
          else (LeafNode(item, newElts), maybeFound)
      }
    }
  }
}

/**
 * a PriorityQueue implemented via PairingHeap
 *
 * PairingHeaps are a heap datastructure giving good amortized performance.
 */

class PairingHeap[A]()(implicit ord: Ordering[A])
        extends Serializable
        with Iterable[A]
        with IterableLike[A, PairingHeap[A]]
        with Growable[A]
        with Shrinkable[A]
        with mutable.Cloneable[PairingHeap[A]]
        with mutable.Builder[A, PairingHeap[A]] {

  protected[this] override def newBuilder = PairingHeap.newBuilder[A]
  override def repr = this

  private var _length: Int       = 0
  private var _head  : PHNode[A] = EmptyNode

  /**
   * add an element to the heap.  Amortized O(1)
   */
  def +=(elem: A): this.type = {
    _head = PHNode.merge(LeafNode(elem, Nil), _head) // order is important.  see merge concerning stability
    _length += 1
    this
  }

  /**
   * adds the TraversableOnce elements into a new clone of this.  O(n) + Amortized O(1) per TraversableOnce element
   */
  def ++(xs: TraversableOnce[A]): PairingHeap[A] = {this.clone() ++= xs}

  /**
   * remove the first matching `elem` in this object if it exists.  O(n)
   */
  def -=(elem: A): this.type = {
    remove(elem)
    this
  }

  /**
   * remove all elements from the object.  O(1)
   */
  def clear() {
    _head = EmptyNode
    _length = 0
  }

  /**
   * provide a clone of the object. O(n)
   */
  override
  def clone(): PairingHeap[A] = {
    val rv = new PairingHeap[A]
    rv._length = _length
    rv._head = _head.clone()
    rv
  }

  /**
   * determine if `elem` is contained in the heap.  O(n)
   */
  def contains(elem: A)(implicit ord: Ordering[A] = ord): Boolean = {PHNode.contains(_head, elem)(ord)}

  /**
   * remove the smallest element of the heap.  Amortized O(log n)
   *
   * Also see inspect().
   */
  def dequeue(): A = _head match {
    case EmptyNode => throw new NoSuchElementException("no element to remove from heap")
    case _ =>
      val node = _head.asInstanceOf[LeafNode[A]]
      _head = PHNode.mergePairs(node.elts)
      _length -= 1
      node.item
  }

  /**
   * remove the smallest element of the heap as an Option.  Amortized O(log n)
   */
  def dequeueOption(): Option[A] = {
    try Some(dequeue())
    catch {case e: Exception => None}
  }

  /**
   * add one or more elements to the heap.  Amortized O(1) per element.
   */
  def enqueue(elems: A*) {this ++= elems}

  /**
   * provide an efficient foreach.  O(n)
   *
   * Note that this does not address the elements in order.  Use
   * {{{
   *   for (i <- pairingHeap.iterator) ...
   * }}}
   * for that.
   */
  override
  def foreach[U](f: A => U) {_head.foreach(f)}

  /**
   * The hashCode method always yields an error, since it is not safe to use mutable queues as keys in hash tables.
   *
   * @return never.
   */
  override
  def hashCode(): Int = throw new UnsupportedOperationException("unsuitable as hash key")

  /**
   * inspect the smallest element of the heap.  O(1)
   *
   * Also see dequeue().
   */
  def inspect(): A = _head match {
    case EmptyNode => throw new NoSuchElementException("no element to remove from heap")
    case _ => _head.asInstanceOf[LeafNode[A]].item
  }

  /**
   * inspect the smallest element of the heap as an Option.  O(1)
   */
  def inspectOption(): Option[A] = {
    try Some(inspect())
    catch {case _: Exception => None}
  }

  /**
   * determine if the heap is empty.  O(1)
   */
  override
  def isEmpty: Boolean = _head.isEmpty

  /**
   * return an iterator over the current elements of the heap.  O(n)
   *
   * Each call to hasNext of the resulting iterator is O(1).  Each next is amortized O(log n).
   */
  def iterator: Iterator[A] = {
    val heap = this

    new Iterator[A] {
      val source: PairingHeap[A] = heap.clone()

      def hasNext: Boolean = source._head.isDefined

      def next(): A = source.dequeue()
    }
  }

  /**
   * return the number of elements in the heap. O(1)
   */
  def length: Int = _length

  /**
   * remove a specific element from the heap if it exists. O(n)
   */
  def remove(elem: A)(implicit ord: Ordering[A] = ord): Option[A] = {
    val (newHead, maybeRv) = PHNode.remove(_head, elem)(ord)

    if (maybeRv.isDefined) {
      _head = newHead
      _length -= 1
    }

    maybeRv
  }

  /**
   * Returns the reverse of this heap. O(n log n)
   *
   * The PairingHeap that gets returned will have an inversed ordering - if for some elements
   * `x` and `y` the original heaps's ordering had `compare` returning an integer ''w'',
   * the new one will return ''-w'', assuming the original ordering abides its contract.
   *
   * Note that the order of the elements will be reversed unless the `compare` method returns 0.
   * In this case, such elements will be subsequent, but their corresponding subinterval may be
   * inappropriately reversed. However, due to the compare-equals contract, they will also be equal.
   *
   * @return   A reversed PairingHeap
   */
  def reverse: PairingHeap[A] = {
    val rv = new PairingHeap[A]()(new Ordering[A] {
      def compare(x: A, y: A) = ord.compare(y, x)
    })
    for (i <- this.iterator) rv += i
    rv
  }

  /**
   * return a result when using a PairingHeap as a builder
   */
  def result(): PairingHeap[A] = clone()

  override
  def toList = this.iterator.toList

  override
  def toString(): String = "PairingHeap<" + size + "> " + _head
}

object PairingHeap {

  def apply[A](xs: A*)(implicit ord: Ordering[A]): PairingHeap[A] = {new PairingHeap[A] ++= xs}

  def empty[A]()(implicit ord: Ordering[A]): PairingHeap[A] = new PairingHeap[A]

  def newBuilder[A](implicit ord: Ordering[A]): mutable.Builder[A, PairingHeap[A]] = new PairingHeap[A]

  implicit def CanBuildFrom[A](implicit ord: Ordering[A])
  : CanBuildFrom[PairingHeap[_], A, PairingHeap[A]] = new CanBuildFrom[PairingHeap[_], A, PairingHeap[A]] {
    def apply(from: PairingHeap[_]) = newBuilder[A]
    def apply() = newBuilder[A]
  }
}
