package com.spotright.common.lib

import scalaz.Monoid

/**
 * calculate a running mean and variance
 *
 * This method (See Knuth TAOCP vol 2, 3rd edition, page 232) avoids
 * loss of precision and storing intermediate values.
 *
 * {{{
 *    M_1 = x, S_1 = 0
 *
 *    M_i = M_i-1 + (x - M_i-1) / i
 *    S_i = S_i-1 + (x - M_i-1) * (x - M_i)
 *
 *    m   = M_i
 *    s^2 = S_i / (i-1)
 * }}}
 *
 * From MathWorld the variance can be calculated directly as
 * {{{
 * {s_i+1}^2 = s_i^2 - s_i^2 / i + (i+1) * (M_i+1 - M_i)^2
 * }}}
 *
 * but using Knuth provides a bit more flexibility.
 */
case class RunningMV(size: Int, mean: Double, sk: Double) {

  /**
   * add a value to the run
   */
  def +(x: Double): RunningMV = {
    val k = size + 1

    if (k == 1) RunningMV(k, x, 0.0)
    else {
      val d = x - mean
      val M = mean + d / k
      RunningMV(k, M, sk + d * (x - M))
    }
  }

  /**
   * subtract a value from the run (i.e., rvm - x === rmv + -x)
   */
  def -(x: Double) = this.+(-x)

  /**
   * (sample) variance
   */
  def variance: Double = if (size < 2) 0.0 else sk / (size - 1)

  /**
   * standard variance
   */
  def standardVariance: Double = if (size < 2) 0.0 else sk / size

  /**
   * (sample) standard deviation
   *
   * This is simply {{{math.sqrt(variance)}}}
   */
  def sd: Double = math.sqrt(variance)

  /**
   * an alias for `size`
   */
  def length: Int = size
}

object RunningMV {

  /**
   * start a run with the given value
   */
  def apply(x: Double): RunningMV = RunningMV(1, x, 0.0)

  /**
   * the starting point for all runs (i.e., MV_0)
   */
  def empty: RunningMV = RunningMV(0, 0.0, 0.0)

  implicit object RunningMVMonoid extends Monoid[RunningMV] {
    def zero: RunningMV = empty

    /**
     * https://www.quora.com/Statistics-How-to-calculate-combined-mean-and-SD-from-2-datasets
     */
    def append(f1: RunningMV, f2: => RunningMV): RunningMV = {
      if (f1.size == 0) f2
      else if (f2.size == 0) f1
      else {
        val nx = f1.size
        val ny = f2.size
        val nz = nx + ny
        val x = f1.mean
        val y = f2.mean
        val z = (f1.size * f1.mean + f2.size * f2.mean) / nz
        val sx2 = f1.standardVariance
        val sy2 = f2.standardVariance

        val sz2 =
          nx * sx2 +
          2 * nx * x * (x - z) +
          nx * (z * z - x * x) +
          ny * sy2 +
          2 * ny * y * (y - z) +
          ny * (z * z - y * y)

        RunningMV(nz, z, sz2)
      }
    }
  }
}
