package com.spotright.chorus.lib

case class Centroid(count: Int, mass: Double) {

  def +(other: Centroid): Centroid = {
    val newCount = count + other.count
    Centroid(newCount, (mass * count + other.mass * other.count) / newCount)
  }
}

object Centroid {

  trait CanShows { self: Centroid =>
    override def toString: String =
      f"[$count, $mass%.4f]"
  }

  val zero: Centroid = Centroid(0, 0.0)

  def apply(n: Double): Centroid = Centroid(1, n)

  object byMass extends Ordering[Centroid] {
    def compare(a: Centroid, b: Centroid): Int = a.mass compare b.mass
  }

  /**
    * Return approximate count of items LT marker.
    */
  def trapazoidCount(marker: Double, left: Centroid, right: Centroid): Double = {
    require(marker >= left.mass && marker <= right.mass, "marker is outside of trapazoid area")

    val halfLeft = left.count / 2.0

    if (marker <= left.mass)
      halfLeft.toDouble
    else if (marker >= right.mass)
      left.count + right.count / 2.0
    else {
      val rise = right.count - left.count
      val run = right.mass - left.mass
      val slope = rise / run

      val markedBase = marker - left.mass
      val markedHeight = markedBase * slope + left.count

      // Area of trapazoid: (base1 + base2) * height / 2 ; bases are parallel

      val observations = (left.count + right.count) / 2
      val leftArea = (left.count + markedHeight) * markedBase / 2
      val fullArea = (left.count + right.count) * (right.mass - left.mass) / 2

      val leftObservations = leftArea / fullArea * observations

      halfLeft.toDouble + leftObservations
    }
  }
}
