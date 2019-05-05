package com.spotright.common.util

import scala.language.implicitConversions

object Stochastic {

  import StochasticImplicits._

  private val rgen = new scala.util.Random()

  /**
   * generate a Poisson random variable
   *
   * Uses Knuth's method.  Best with small lambda (< 30 or so).
   */
  def nextPoisson(lambda: Double): Int = rgen.nextPoisson(lambda)

  /** generate an Exponential random variable */
  def nextExponential(lambda: Double): Double = rgen.nextExponential(lambda)

  class PimpedRandom(rgen: scala.util.Random) {

    /**
     * generate a Poisson random variable
     *
     * Uses Knuth's method.  Best with small lambda < 30 or so.
     */
    def nextPoisson(lambda: Double): Int = {
      require(lambda >= 0.0, "lambda < 0.0")
      val p = math.exp(-lambda)

      @annotation.tailrec
      def step(q: Double, n: Int): Int = if (q < p) n else step(q * rgen.nextDouble, n + 1)

      step(math.random, 0)
    }

    /** generate an Exponential random variable */
    def nextExponential(lambda: Double): Double = -math.log(rgen.nextDouble)/lambda
  }
}

object StochasticImplicits {

  implicit def random2Pimped(rgen: scala.util.Random): Stochastic.PimpedRandom = new Stochastic.PimpedRandom(rgen)
}
