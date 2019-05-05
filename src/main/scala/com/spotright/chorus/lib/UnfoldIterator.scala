package com.spotright.common.lib

/**
  * Provides unfold in Iterator format.
  *
  * {{{
  * val iter = new UnfoldItator(0, (s: Int) => if (s < 10) Option(s -> s + 1) else None)
  * iter.toList  // List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  * }}}
  *
  * @param init Initial state
  * @param f    A function possibly transforming state and providing a result
  * @tparam S State type
  * @tparam A Result type
  */
class UnfoldIterator[S, A](init: S, f: S => Option[(A, S)]) extends Iterator[A] {
  private var s       = init
  private var mnext   = Option.empty[A]
  private var canNext = true

  def next(): A = {
    if (!hasNext) Iterator.empty.next()

    val v = mnext.get
    mnext = None
    v
  }

  def hasNext: Boolean = {
    if (canNext) {
      mnext =
        mnext orElse
          f(s).map {
            case (a, ns) =>
              s = ns
              a
          }

      canNext = mnext.isDefined
    }

    canNext
  }

}

/**
  * Companion for UnfoldIterator providing helpers.
  */
object UnfoldIterator {

  /** Value-like constructor. */
  def apply[S, A](init: S, f: S => Option[(A, S)]): Iterator[A] =
    new UnfoldIterator(init, f)

  /** Constructor for: (S => Option[(A,S)]) => S => Iterator[A] */
  def unfoldf[S, A](f: S => Option[(A, S)]): S => Iterator[A] =
    (init: S) => UnfoldIterator(init, f)

  /**
    * Iterate over unfold-ed pages.
    *
    * {{{
    * case class Cursor(client: Client, pos: Long)
    *
    * val iter: Iterator[Row] =
    *   paged(
    *     Cursor(db, 0L),
    *     { case (db, pos) =>
    *         val p = db.getPage(pos)
    *         val nextPos = p.nextPosition
    *         if (nextPos == pos)
    *           None
    *         else
    *           Option(p -> Cursor(db, nextPos) },
    *     (p: Page) => p.rows.iterator
    *   )
    * }}}
    *
    * @param init Initial state
    * @param f    Function possibly transforming state and provide a page
    * @param g    Function transforming a page to an iterator of results
    * @tparam S State type
    * @tparam P Page type
    * @tparam A Result type
    */
  def paged[S, P, A](init: S, f: S => Option[(P, S)], g: P => Iterator[A]): Iterator[A] =
    UnfoldIterator(init, f).flatMap(g)
}
