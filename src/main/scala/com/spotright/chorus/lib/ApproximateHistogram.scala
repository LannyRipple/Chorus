package com.spotright.chorus.lib

import scala.util.Try

import com.spotright.common.lib.PairingHeap
import com.spotright.chorus.lib.SListWalker.implicits._

/**
  * Build an approximate Histogram.
  * Record a fixed number of Centroids merging the closest to make room for new values.
  * Build trapazoids to get approximate bucket counts.
  *
  * @param pointCount Point at which new elements are merged
  * @param flushDirty How dirty the heap has to be to rebuild.
  * @see https://metamarkets.com/2013/histograms/
  */
class ApproximateHistogram(pointCount: Int, flushDirty: Double = 0.5) {

  var _totalCount = 0  // total elements in histogram

  def totalCount(): Int = _totalCount

  private
  var centsCount = 0  // individual centroid count

  protected
  var cents = SList.empty[Centroid]

  private
  var associatedDistances = Map.empty[(Double,Double), CentroidDistance]

  private def adKey(cd: CentroidDistance): (Double, Double) = (cd.left, cd.right)
  private def adKey(left: Centroid, right: Centroid): (Double, Double) = (left.mass, right.mass)

  private
  val distanceHeap = PairingHeap.empty[CentroidDistance]

  def log(msg: String): Unit = println(s";; $msg")

  def debug(): Unit = {
    val cs = cents.map { c => new Centroid(c.count, c.mass) with Centroid.CanShows }.mkString(", ")
    println(s";; cents = $cs")

    val ds = associatedDistances.toList.sortBy{ case ((left, _), _) => left}.map{ case ((left, right), dist) =>
      val v = s"($left, $right)"
      s"$v -> ${dist.distance}"}.mkString("\n")
    println(s";; ads = $ds")

    val mit = {
      var x  = distanceHeap.inspectOption()
      while (x.exists{_.isDirty}) {
        distanceHeap.dequeue()
        x  = distanceHeap.inspectOption()
      }
      x
    }

    mit.foreach{it =>
      println(s";; shortestDist = (${it.left}, ${it.right}) -> ${it.distance} dirty=${it.isDirty}")
    }
  }

  private
  def insert(elem: Centroid): Unit = {
    _totalCount += 1

    val walker = cents.walker.rightWhile{_.mass < elem.mass}
    val mlistElem = walker.get

    if ( ! mlistElem.fold(true){listElem => listElem.mass != elem.mass } ) {
      walker.update(mlistElem.get + elem) // does not increase centsCount
    }
    else {
      centsCount += 1

      walker.insert(elem)
      cents = walker.result

      // We need to
      // 1. Calculate left and right distances (whichever exist).
      // 2. Mark previous distance involving left/right centroids as dirty.
      // 3. Associate new distances with centroids and add distances to heap.
      // 4. Find smallest distance in heap.  (Removing some dirty distances from heap in process.)
      // 5. Mark smallest distance as dirty (and remove from heap; consequence of 7).
      // 6. Find centroids with smallest distance and mark assocaited dirty.
      // 7. Merge smallest distance centroids.
      // 8. Associate new distances with centroids and add distances to heap.

      // cents: ... J -> K -> L -> M -> ...
      // dists:       JK   KL   LM
      //
      // insert:... J -> K -> Z -> L -> M -> ...
      // dirty:            KL
      // add:              KZ   ZL
      //
      // Assume ZL is smallest distance
      //
      // combine:   J -> K -> Y -> M -> ...  Y == ZL
      // dirty:            KZ ZL LM
      // add:              KY   YM

      // (1 - 3)
      if (distanceHeap.length > 0) {
        val WorkingData(postInsKeys, postInsDists) = postInsertData(walker.state())
        postInsKeys.foreach(markDirty)
        postInsDists.foreach(putOnHeap)
      }

      // (4 - 8)
      mergeCentroids()
    }
  }

  private
  def mergeCentroids(): Unit = {
    if (centsCount > pointCount) {

      // Once the heap is built then `length` will always be > 0.
      if (distanceHeap.length == 0) {
        rebuildHeap()
      }

      // If the number of dirty heap elements gets too high rebuild the heap
      if ((distanceHeap.length - centsCount).toDouble / centsCount >= flushDirty) {
        rebuildHeap()
      }

      while (centsCount > pointCount) {

        // (4)
        val smallestDistance = {

          // We've added things (above) to heap so it cannot be empty.
          while (distanceHeap.inspect().isDirty)
            distanceHeap.dequeue()

          distanceHeap.dequeue()
        }

        // (5)
        markDirty(smallestDistance)

        // (6)
        val merger = cents.walker.rightWhile {
          _.mass < smallestDistance.left
        }

        val WorkingData(preMergeKeys, _) = preMergeData(merger.state())
        preMergeKeys.foreach(markDirty)

        // (7)
        merger.update(merger.head + merger.index.next.head) // merge list elts N, N+1
        merger.index.next = merger.index.next.next // drop list elt N+1
        cents = merger.result
        centsCount -= 1

        // (8)
        val WorkingData(_, postMergeDists) = postMergeData(merger.state())
        postMergeDists.foreach(putOnHeap)
      }
    }
  }

  def markDirty(key: (Double,Double)): Unit = {
    associatedDistances.get(key).foreach{_.markDirty()}
    associatedDistances -= key
  }

  def markDirty(distance: CentroidDistance): Unit = markDirty(distance.left -> distance.right)

  def putOnHeap(distance: CentroidDistance): Unit = {
    associatedDistances += adKey(distance) -> distance
    distanceHeap += distance
  }

  case class WorkingData(dirtyKeys: List[(Double,Double)], dists: List[CentroidDistance])

  /*
   * On an insert into the cents we start with
   *
   * cents:    J - K
   * dists:     JK
   *
   * and finish with
   *
   * cents:    J - X - K
   * dists:     JX  XK
   *
   * SListWalker index will be at X.
   * JK must be marked dirty.  JX and XK must be added to the heap.
   * Either J or K might not exist if X is inserted on either end.
   */
  def postInsertData(walkerState: SListWalker.State[Centroid]): WorkingData = {
    val mleftDist =
      walkerState.previous.map { seen =>
        CentroidDistance(seen.head, seen.next.head)
      }

    val mrightDist =
      Try {
        val index = walkerState.index
        CentroidDistance(index.head, index.next.head)
      }.toOption

    val keyBuilder = List.newBuilder[(Double,Double)]

    (
      for {
        leftDist <- mleftDist
        rightDist <- mrightDist
      } yield
        (leftDist.left, rightDist.right) // leftDist.right == rightDist.left == newly inserted elem
      ).foreach { key =>
      keyBuilder += key
    }

    val distBuilder = List.newBuilder[CentroidDistance]

    // (3, 4)
    List(mleftDist, mrightDist).foreach { mcd =>
      mcd.foreach { cd =>
        distBuilder += cd
      }
    }

    WorkingData(keyBuilder.result(), distBuilder.result())
  }

  /*
   * We have found the smallest distance and will merge centroids.
   * First we must find keys for soon-to-be dirty distances.
   * Assume smallest distance is KL.
   *
   * cents:       J - K - L - M
   * dists:        JK  KL  LM
   *
   * SListWalker index will be at K.
   * KL gets handled explicitly as smallest distance.
   * JK and KL must be marked dirty.
   * Either J or M might not exist if we are on end.
   *
   * @see postMergeData
   */
  def preMergeData(mergerState: SListWalker.State[Centroid]): WorkingData = {
    val mleftKey =
      mergerState.previous.map { seen =>
        adKey(seen.head, seen.next.head)
      }

    val mrightKey =
      Try {
        val index = mergerState.index
        adKey(index.head, index.next.head)
      }.toOption

    val keyBuilder = List.newBuilder[(Double,Double)]

    List(mleftKey, mrightKey).foreach { mkey =>
      mkey.foreach { key =>
        keyBuilder += key
      }
    }

    WorkingData(keyBuilder.result(), List.empty[CentroidDistance])
  }

  /*
   * We have found the smallest distance and have merged centroids.
   *
   * cents:       J - X - M
   * dists:        JX   XM
   *
   * SListWalker index will be at X.
   * Distance JX and XM must be added to the heap.
   * Either J or M might not exist if we are on end.
   *
   * @see preMergeData
   */
  def postMergeData(mergerState: SListWalker.State[Centroid]): WorkingData = {
    val mleftDist =
      mergerState.previous.map { seen =>
        CentroidDistance(seen.head, seen.next.head)
      }

    val mrightDist =
      Try {
        val index = mergerState.index
        CentroidDistance(index.head, index.next.head)
      }.toOption

    val distBuilder = List.newBuilder[CentroidDistance]

    List(mleftDist, mrightDist).foreach { mcd =>
      mcd.foreach { cd =>
        distBuilder += cd
      }
    }

    WorkingData(List.empty[(Double,Double)], distBuilder.result())
  }

  private
  def rebuildHeap(): Unit = {
    gc()

    cents.iterator.sliding(2).foreach {
      case Seq(left, right) =>
        val dist = CentroidDistance(left, right)

        // add it to distances
        associatedDistances += adKey(left, right) -> dist

        // add it to heap
      distanceHeap += dist
    }
  }

  /**
    * Possibly reduce memory footprint of instance
    * by cleaning up transient data.
    *
    * Calling this and adding new elements will recreate
    * the transient data so it should not be called until
    * the ApproximateHistogram has consumed all the data
    * points.
    */
  def gc(): Unit = {
    distanceHeap.clear()
    associatedDistances = Map.empty[(Double, Double), CentroidDistance]
  }

  def add(elem: Double): ApproximateHistogram = {
    insert(Centroid(elem))
    this
  }

  def merge(other: ApproximateHistogram): Unit = {
    distanceHeap.clear()
    associatedDistances = Map.empty[(Double, Double), CentroidDistance]

    _totalCount += other.totalCount()
    cents = cents.merge(SList(other.cents.iterator))(Centroid.byMass)

    mergeCentroids()
  }

  /**
    * Report the number of items entered into the histogram
    * that are (approximately) less than the point(s) value(s).
    *
    * If using more than one point the points should be in order.
    * You will get accurate point counts if this is not the case
    * but you won't be able to (easily) diff the point counts to
    * get bucket counts.
    *
    * Note that point counts / totalCounts gives percentiles.
    */
  def cumulativeCounts(points: Seq[Double]): IndexedSeq[Double] = {
      var top = cents.walker
      var sum = 0.0
      var lastPrev = Option.empty[SList[Centroid]]

      points.map { p =>
        // Take care of any points before the histogram starts
        if (p < cents.head.mass)
          0.0
        else {
          if (lastPrev.fold(true){list => p < list.head.mass}) {
            top = cents.walker
            sum = 0.0
            lastPrev = None
          }

          // We need two centroids remaining for trapazoid distance.
          while (top.index.next.nonEmpty && p >= top.head.mass) {
            sum += lastPrev.fold(0.0){seen => seen.head.count.toDouble}
            top.right
            lastPrev = top.previous
          }

          val rv =
          if (top.index.next.isEmpty)
            sum
          else {
            top.previous.fold(0.0) { left =>
              sum + Centroid.trapazoidCount(p, left.head, top.index.head)
            }
          }

          rv
        }
      }(collection.breakOut)
    }
}
