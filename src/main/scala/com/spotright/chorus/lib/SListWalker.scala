package com.spotright.chorus.lib

import scala.collection.AbstractIterator

/**
  * Traverse and insert into an SList while allowing access the `previous`
  * node to an insert.
  *
  * The following attributes can be used to access the underlying list state.
  *
  * $ result : The head of the list.  Might not be the original head if elements
  * were inserted into the list.
  * $ previous : The SList node previous to the current `index`.  Will be None
  * at the head of the list.
  * $ index : The SList node that insert/update act on.
  */
class SListWalker[A](list: SList[A]) {
  import SListWalker.State

  private
  var _state = State(list)

  def state(): State[A] = _state

  def result: SList[A] = _state.result
  def previous: Option[SList[A]] = _state.previous
  def index: SList[A] = _state.index

  def isEOL: Boolean = index.isEmpty
  def nonEOL: Boolean = !isEOL

  def get: Option[A] = index.get
  def head: A = index.head

  /**
    * Advance the position by cons.
    */
  def right: SListWalker[A] = {
    if (isEOL)
      this
    else {
      _state = _state.copy(previous = Option(index), index = index.next)
      this
    }
  }

  /**
    * Advance the index position as long as `pred` is true.
    *
    * @param pred Predicate on the current position's elem value.
    */
  def rightWhile(pred: A => Boolean): SListWalker[A] = {
    while (nonEOL && pred(index.head)) {
      right
    }
    this
  }

  /**
    * Advance the index position to the end of the SList.
    */
  def eol: SListWalker[A] = rightWhile{ _ => true}

  /**
    * Insert a new element into the SList at the current index.
    */
  def insert(elem: A): SListWalker[A] = {
    val rv =
    previous.fold {
      _state = State(new SList(elem, index))
      this
    } { seen =>
      val list = new SList(elem, index)
      seen.next = list

      _state = _state.copy(index = list)
      this
    }

    rv
  }

  def insert(list: SList[A]): SListWalker[A] = {
    previous.fold {  // no `previous` so at start of list
      if (isEOL) {
        _state = State(list)
        this
      }
      else {
        val orig = result
        _state = State(list)

        eol
        insert(orig)
        rewind
      }
    } { seen =>  // somewhere in the list
      if (isEOL) {
        seen.next = list

        _state.copy(index = list)
        this
      }
      else {
        val right = seen.next
        seen.next = list

        SListWalker(list).eol.insert(right) // `list` ::: `right`

        _state = _state.copy(index = list)
        this
      }
    }
  }

  /**
    * Update the elem value of the current index.
    */
  def update(elem: A): SListWalker[A] = {
    require(nonEOL, "cannot update EOL element")
    index.elem = elem
    this
  }

  /**
    * Reset SListInserted to head of SList
    */
  def rewind: SListWalker[A] = {
    _state = State(result)
    this
  }
}

object SListWalker {

  case class State[A](result: SList[A], previous: Option[SList[A]], index: SList[A])

  object State {
    def apply[A](list: SList[A]): State[A] = State(list, None, list)
  }

  def apply[A](list: SList[A]): SListWalker[A] = new SListWalker(list)

  object implicits {

    implicit class RichSListWalker[A](val list: SList[A]) extends AnyRef with Iterable[A] {

      def walker: SListWalker[A] = SListWalker(list)

      def iterator: Iterator[A] = new AbstractIterator[A] {
        val ins = SListWalker(list)

        override def hasNext: Boolean = ins.nonEOL

        override def next(): A = {
          if (!hasNext)
            Iterator.empty.next()
          else {
            val rv = ins.head
            ins.right
            rv
          }
        }
      }

      def filterInPlace(p: A => Boolean): SList[A] = {
        var base = list

        // drop anything at start that filters
        while (base.nonEmpty && !p(base.head)) {
          base = base.next
        }

        var prev = base

        // assert: if node.isEmpty then node.next == node

        while (prev.next.nonEmpty) {
          if (p(prev.next.head))
            prev = prev.next
          else
            prev.next = prev.next.next // prune it from the list
        }

        base
      }

      def mapInPlace(f: A => A): SList[A] = {
        var top = list
        while (top.nonEmpty) {
          top.elem = f(top.elem)
          top = top.next
        }
        list
      }

      def flatMap[B](f: A => SList[B]): SList[B] = {
        list.iterator
          .foldLeft(SListWalker(new SList[B])) { (acc, b) =>
            acc.insert(f(b)).eol
          }
          .result
      }

      /**
        * Merge two sorted SLists.
        *
        * Elements of `list` are preferred on matches.
        */
      def merge(other: SList[A])(implicit ord: Ordering[A]): SList[A] = {

        if (list.isEmpty)
          other
        else if (other.isEmpty)
          list
        else {
          var b = other  // cannot mix val and var with: (top, b) = ...

          val top =
            if (ord.compare(list.head, other.head) < 1)
              list
            else {
              b = list
              other
            }

          var prev = top

          while (prev.next.nonEmpty && b.nonEmpty) {
            // prev(A) -> prev.next(C) -> prev.next.next(+) ->
            //                    b(B) ->         b.next(*) ->
            //
            // - OR -
            //
            // prev(A) -> prev.next(B) -> prev.next.next(+) ->
            //                    b(C) ->         b.next(*) ->

            if (ord.compare(prev.next.head, b.head) == 1) {
              val t = b
              b = b.next

              // prev(A) -> prev.next(C) -> prev.next.next(+) ->
              //                    t(B) ->              b(*) ->

              t.next = prev.next
              prev.next = t

              // prev(A) -> prev.next:t(B) -> prev.next.next(C) -> prev.next.next.next(+) ->
              //                                                                     b(*) ->
            }

            prev = prev.next

            // prev:t(B) -> prev.next(C) -> prev.next.next(+) ->
            //                                           b(*) ->
            // - OR -
            //
            // prev(B) -> prev.next(+) ->
            //                    b(C) -> b.next(*) ->
          }

          if (prev.next.isEmpty)
            prev.next = b

          top
        }
      }
    }
  }
}
