package com.spotright.chorus.lib

case class CentroidDistance(left: Double, right: Double, var isDirty: Boolean = false)
  extends Ordered[CentroidDistance]{

  val distance: Double = right - left

  def compare(other: CentroidDistance): Int =
    distance compare other.distance

  def markDirty(): Unit = isDirty = true
}

object CentroidDistance {

  def apply(left: Centroid, right: Centroid): CentroidDistance =
    CentroidDistance(left.mass, right.mass)
}
