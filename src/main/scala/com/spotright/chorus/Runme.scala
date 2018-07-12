package com.spotright.chorus

import scala.collection.immutable.TreeMap

import com.spotright.chorus.lib._

object Runme {

  def run = 1000000
  def sigma = 2.6
  def mu = 2.5

  def main(av: Array[String]): Unit = {

    val size = av.lift(0).map{s => Integer.parseInt(s)}.getOrElse(200)

    val ah = new ApproximateHistogram(size)
    val ah2 = new ApproximateHistogram(size)

    (1 to run/2).foreach { n =>
      val z = scala.util.Random.nextGaussian()
      val y = scala.util.Random.nextGaussian()

      def x(z: Double) = z * sigma + mu

      if (ah.totalCount % 1000 == 0)
        print(".")

      ah.add(x(z))
      ah2.add(x(y))
    }

    ah.merge(ah2)

    println()
    println(s"added = ${ah.totalCount()}")

    println()
    ah.gc()
    ah.debug()
    println()

    val points = 0L to 1000L by 100L
    val cc = ah.cumulativeCounts(points.map{_.toDouble})
    println("__Cumulative Count__")
    println(points.zip(cc).mkString(", "))
    println("__Cumulative Percentile__")
    println(points.zip(cc.map{100.0 * _.toDouble / ah.totalCount()}).mkString(", "))
  }
}
