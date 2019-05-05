package com.spotright.common.lib

/**
  * Similar to groupBy but will return the same groupings each time (depending on `f`).
  *
  * Motivating example - Given some original Seq which records its elements index position then
  * groupBy will give you batches of elements.  If some of the Seq is consumed then a new groupBy
  * will change the batching.  FencedIterator relies on `f` which provides the index positions.
  * The code will then return the original batching even if some elements get consumed between
  * instances of FencedIterator.
  *
  * Concretely a FencedIterator forms batches such that
  * {{{
  *   val idx = f(a_N)
  *   val q = idx / batchSize
  *
  *   // Where batch elements share `q`
  * }}}
  *
  * @note `elts` should be sortedBy `f` to get meaningful results between FencedIterator instances
  */
class FencedIterator[A](elts: TraversableOnce[A], batchSize: Int)(f: A => Int) extends Iterator[IndexedSeq[A]]{
  val underlaying = elts.toIterator.buffered

  def hasNext: Boolean = underlaying.hasNext

  def next(): IndexedSeq[A] = {
    if (!hasNext)
      Iterator.empty.next()

    val q = f(underlaying.head) / batchSize
    val bldr = IndexedSeq.newBuilder[A]

    while (underlaying.hasNext && f(underlaying.head) / batchSize == q) {
      bldr += underlaying.next()
    }

    bldr.result()
  }

}
