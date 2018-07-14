package com.spotright.chorus.lib

import scala.collection.AbstractIterator

case class AAN[A](a: A, k: Int, left: AAN[A] = null, right: AAN[A] = null) {
  override def toString: String = {
    val ls = if (left == null) "*" else left.toString
    val rs = if (right == null) "*" else right.toString
    s"AAN($a,$k,$ls,$rs)"
  }
}

/**
  * An immutable, balanced AATree.
  *
  * @param ord An ordering of the tree's values.
  * @note A Map[K,V] can be represented using a key of {{{case class Value[K,V](key: K, value: Option[V])}}}
  *       and an Ordering of {{{Ordering.on{kv => kv.key}}}
  * @see http://user.it.uu.se/~arnea/ps/simp.pdf
  * @see https://en.wikipedia.org/wiki/AA_tree
  */
class AATree[A](val ord: Ordering[A]) extends AANOps[A] with Iterable[A] {

  import AATree._

  type AppendF = (A,A) => A
  final val KEEP: AppendF = {(previous: A, _: A) => previous}
  final val REPLACE: AppendF = {(_: A, offered: A) => offered}

  private var _tree: AAN[A] = _

  def tree: AAN[A] = _tree
  override def size: Int = _size

  override def isEmpty: Boolean = isNil(_tree)

  /**
    * complexity: O(log N)
    */
  def get(a: A): Option[A] =
    Option(search(a, _tree)).map{_.a}

  def minimum: Option[A] = AATree.minimum(tree)
  def maximum: Option[A] = AATree.maximum(tree)

  /**
    * Insert a new value into the tree.
    *
    * complexity: O(log N)
    *
    * @param a Value to be inserted.
    * @param appendf Function to append the previous and offered values in case of collision.
    * @see Functions `KEEP` and `REPLACE`.
    */
  def insert(a: A, appendf: AppendF = REPLACE): AATree[A] = {
    _tree = insert(a, _tree, appendf)
    this
  }

  /**
    * complexity: O(N_other * log( N_other + N_this ))
    */
  def merge(other: AATree[A], appendf: AppendF = REPLACE): AATree[A] = {
    _tree =
      other.iterator.foldLeft(tree){
        case (t, a) =>
          insert(a, t, appendf)
      }

    this
  }

  /**
    * Delete the given value from the tree if it exists.
    *
    * complexity: O(log N)
    */
  def delete(a: A): AATree[A] = {
    _tree = delete(a, _tree)._2
    this
  }

  /**
    * Delete the given value from the tree if it exists returning
    * the value found in the tree.
    *
    * complexity: O(log N)
    */
  def remove(a: A): Option[A] = {
    val (rv, t) = delete(a, _tree)
    _tree = t
    rv
  }

  /**
    * Return an iterator of the tree's values.
    *
    * complexity: O(N)
    */
  def iterator: Iterator[A] = new AbstractIterator[A] {
    var stack: List[Either[AAN[A], A]] =
      if (isNil(tree))
        List()
      else if (isLeaf(tree))
        List(Right(tree.a))
      else if (isNil(tree.left))
        List(Right(tree.a), Left(tree.right))
      else if (isNil(tree.right))
        List(Left(tree.left), Right(tree.a))
      else
        List(Left(tree.left), Right(tree.a), Left(tree.right))

    def hasNext: Boolean = stack.nonEmpty

    def next(): A = {
      val (rv, stackStar) = stack match {
        case Nil => (Iterator.empty.next, stack)

        case Right(a) :: rest => (a, rest)

        case Left(t) :: rest =>
          if (isLeaf(t))
            (t.a, rest)
          else if (isNil(t.left))
            (t.a, Left(t.right) :: rest)
          else if (isNil(t.right)) {
            stack = Left(t.left) :: Right(t.a) :: rest
            (next(), stack)
          }
          else {
            stack = Left(t.left) :: Right(t.a) :: Left(t.right) :: rest
            (next(), stack)
          }
      }

      stack = stackStar
      rv
    }
  }
}

trait AANOps[A] {
  self: AATree[A] =>

  import AATree._

  protected var _size: Int = 0

  def search(a: A, t: AAN[A]): AAN[A] = {
    withTree(t){ t =>
      ord.compare(a, t.a) match {
        case -1 => search(a, t.left)
        case 1 => search(a, t.right)
        case 0 => t
      }
    }
  }

  def insert(a: A, t: AAN[A], appendf: AppendF): AAN[A] = {
    if (!isNil(t))
      insert_descending(a, t, appendf)
    else {
      _size += 1
      AAN(a, 1)
    }
  }

  def insert_descending(a: A, t: AAN[A], appendf: AppendF): AAN[A] = {
    val w_insert =
      ord.compare(a, t.a) match {
        case -1 => t.copy(left = insert(a, t.left, appendf))
        case 1 => t.copy(right = insert(a, t.right, appendf))
        // Overwrite a value that already exists.
        case 0 =>
          // OPTIMIZATION
          if (appendf eq KEEP)
            t
          else
            t.copy(a = appendf(t.a, a))
      }

    val w_skew = skew(w_insert)
    val w_split = split(w_skew)

    w_split
  }

  def delete(a: A, t: AAN[A]): (Option[A], AAN[A]) = {
    if (isNil(t))
      None -> t
    else {
      delete_descending(a, t)
    }
  }

  def delete_descending(a: A, t: AAN[A]): (Option[A], AAN[A]) = {
    val (rv, w_delete) =
      ord.compare(a, t.a) match {
        case -1 =>
          val (rv, nl) = delete(a, t.left)
          rv -> t.copy(left = nl)
        case 1 =>
          val (rv, nr) = delete(a, t.right)
          rv -> t.copy(right = nr)
        case 0 =>
          // if we are a leaf easy, otherwise reduce to leaf case
          if (isLeaf(t)) {
            _size -= 1
            Option(t.a) -> null
          }
          else if (isNil(t.left)) { // no predecessor so swap with successor
            val leaf = successor(t)
            Option(t.a) -> t.copy(a = leaf.a, right = delete(leaf.a, t.right)._2)
          }
          else {
            val leaf = predecessor(t)
            Option(t.a) -> t.copy(a = leaf.a, left = delete(leaf.a, t.left)._2)
          }
      }

    rv -> rebalance(w_delete)
  }

}

object AATree {
  // Defs that do not rely on the instance.

  /**
    * Verify that the tree meets AAList constraints.
    *
    * The invariants on an AAList are
    *
    * $ No left node can be at the same level as its parent.  (No horizontal left links.)
    * $ No right node and its right node can be at the same level as the parent. (No consecutive horizontal links.)
    *
    * complexity: O(N)
    */
  def verify[A](t: AAN[A]): Boolean = {
    if (isNil(t) || isLeaf(t))
      true
    else {
      val lp = verify(t.left)
      val rp = verify(t.right)

      val p =
        t match {
          case AAN(_, k, AAN(_, lk, _, _), AAN(_, rk, _, AAN(_, rrk, _, _))) =>
            k == (lk + 1) && !(k == rk && k == rrk) && (k == rk || k == (rk + 1)) && (rk == rrk || rk == (rrk + 1))
          case AAN(_, k, AAN(_, lk, _, _), AAN(_, rk, _, _)) =>
            k == (lk + 1) && (k == rk || k == (rk + 1))
          case AAN(_, k, AAN(_, lk, _, _), _) =>
            k == (lk + 1)
          case AAN(_, k, _, AAN(_, rk, _, _)) =>
            k == rk || k == (rk + 1)
          case _ => true
        }

      lp && rp && p
    }
  }

  /*
   * In the method docs when there is a "horizontal link" (i.e., <- or ->)
   * this indicates that level `k` has the same value in each node.
   */

  def isNil[A](t: AAN[A]): Boolean = t == null
  def isLeaf[A](t: AAN[A]): Boolean = (t.left == null) && (t.right == null)

  def withTree[A](t: AAN[A])(body: AAN[A] => AAN[A]): AAN[A] =
    if (isNil(t)) t else body(t)

  /**            |
    *            v
    *       L <- T
    *      / \    \
    *     A   B    R    becomes
    *
    *       |
    *       v
    *       L -> T
    *      /    / \
    *     A    B   R
    *
    */
  def skew[A](in: AAN[A], rebalancing: Boolean = false): AAN[A] = {
    withTree(in) {
      // OPTIMIZATION: A skew can set up a situation that the next split will undo.
      // Here we have inserted L as a left-horizontal on an existing right-horizontal.
      // Skew will rotate L up, but the coming split will then rotate L back down (plus
      // level change in T)
      //
      //        |
      //        v
      //   L <- T -> R
      //
      // As such we want to avoid the rotation and perform the level change.  The split
      // will then reduce to a predicate which will be false.
      //
      //        |
      //        v
      //        T (k += 1)
      //       / \
      //      L   R
      case t @ AAN(_, k, AAN(_, lk, _, _), AAN(_, rk, _, _)) if !rebalancing && k == lk && k == rk =>
        t.copy(k = k + 1)

      // Swap the pointers of horizontal left links.
      case t @ AAN(_, k, l @ AAN(_, lk, _, lr), _) if k == lk =>
        l.copy(right = t.copy(left = lr))

      case t => t
    }
  }

  /**      |
    *      v
    *      T -> R -> X
    *     /    /
    *    A    B           becomes
    *
    *      |
    *      v
    *      R
    *     / \
    *    T   X
    *   / \
    *  A   B
    *
    */
  def split[A](in: AAN[A]): AAN[A] = {
    withTree(in) {
      // We have two horizontal right links.  Take the middle node, elevate it, and return it.
      case t @ AAN(_, k, _, r @ AAN(_, rk, rl, rr @ AAN(_, rrk, _, _))) if k == rrk =>
        r.copy(k = rk + 1, left = t.copy(right = rl), right = rr)

      case t => t
    }
  }

  def rebalance[A](t: AAN[A]): AAN[A] = {
    // Rebalance the tree. Decrease the level of all nodes in this level if
    // necessary, and then skew and split all nodes in the new level.

    withTree(t){ t =>
      val w_decrease_levels = decrease_levels(t)

      if (w_decrease_levels == t) {
        // OPTIMIZATION: If decrease_levels did not change anything the else
        // clause will end up finding no work to do.  We can avoid all the tests
        // to see if that work is needed by short-circuiting here.
        t
      }
      else {
        val w_skew = skew(w_decrease_levels, rebalancing = true)
        val w_r_skew = r_skew(w_skew)
        val w_rr_skew = rr_skew(w_r_skew)

        val w_split = split(w_rr_skew)
        val w_r_split = r_split(w_split)

        w_r_split
      }
    }
  }

  // rebalance helper.  delete code ensures no Nils
  def decrease_levels[A](t: AAN[A]): AAN[A] = {
    val lk = if (t.left == null) 0 else t.left.k
    val rk = if (t.right == null) 0 else t.right.k

    val should_be = (lk min rk) + 1

    if (should_be < t.k) {
      if (should_be < rk)
        t.copy(k = should_be, right = t.right.copy(k = should_be))
      else
        t.copy(k = should_be)
    }
    else
      t
  }

  // rebalance helper.  delete code ensures no Nils
  def r_skew[A](t: AAN[A]): AAN[A] = t.copy(right = skew(t.right, rebalancing = true))

  // rebalance helper.  delete code ensures no Nils
  def rr_skew[A](in: AAN[A]): AAN[A] = {
    in match {
      case t @ AAN(_, _, _, r @ AAN(_, _, _, rr)) =>
        t.copy(right = r.copy(right = skew(rr, rebalancing = true)))
      case x => x
    }
  }

  // rebalance helper.  delete code ensures no Nils
  def r_split[A](t: AAN[A]): AAN[A] = {
    t.copy(right = split(t.right))
  }

  def minimum[A](t: AAN[A]): Option[A] = {
    if (isNil(t))
      None
    else {
      var l = t
      while (!isNil(l.left))
        l = t.left
      Option(l.a)
    }
  }

  def maximum[A](t: AAN[A]): Option[A] = {
    if (isNil(t))
      None
    else {
      var r = t
      while (!isNil(r.right))
        r = r.right
      Option(r.a)
    }
  }

  def successor[A](t: AAN[A]): AAN[A] = {
    // Tree constraints gaurantee !isNil(t.right)
    var succ = t.right
    while (!isNil(succ.left)) {
      succ = succ.left
    }
    succ
  }

  def predecessor[A](t: AAN[A]): AAN[A] = {
    // Tree constraints gaurantee !isNil(t.left)
    var pred = t.left
    while (!isNil(pred.right)) {
      pred = pred.right
    }
    pred
  }
}
