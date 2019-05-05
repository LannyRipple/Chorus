package com.spotright.common.lib

import scala.collection.mutable

/**
 * A builder-like class for a RunningMV of the last `size` entries.
 */
class MovingMV(size: Int) {
  require(size > 1, "size <= 1")

  private var mv = RunningMV.empty
  private var queue = mutable.Queue.empty[Double]
  private var dirty: Boolean = false

  def +=(x: Double): this.type = {
    if (queue.size == size) queue.dequeue

    queue += x
    dirty = true

    this
  }

  def ++=(xs: TraversableOnce[Double]): this.type = {
    for (x <- xs) this += x
    this
  }

  def clear() {
    mv = RunningMV.empty
    queue = mutable.Queue.empty[Double]
    dirty = false
  }

  /**
   * Produces a RunningMV from the added elements.  After this operation the class
   * can continue accepting more elements (and providing the running average of the
   * last `size` entries via result()).
   */
  def result(): RunningMV = {
    if (dirty) {
      mv = queue.foldLeft(RunningMV.empty){_ + _}
      dirty = false
    }

    mv
  }
}
