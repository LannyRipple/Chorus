package com.spotright.common.lib

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.Random

object ResevoirSampling {

  val rng = new scala.util.Random()

  def sample[T, CC[X] <: TraversableOnce[X]](xs: CC[T], sampleSize: Int, rng: Random = this.rng)
                                            (implicit bf: CanBuildFrom[CC[T], T, CC[T]]) : CC[T] = {
    val iter = xs.toIterator
    var c = sampleSize
    val ys = Vector.newBuilder[T]

    while (iter.hasNext && c > 0) {
      c -= 1
      ys += iter.next()
    }

    var pool = ys.result()

    if (iter.hasNext) {
      c = sampleSize

      iter.foreach {
        item =>
          c += 1
          val j = rng.nextInt(c)
          if (j < sampleSize)
            pool = pool.updated(j, item)
      }
    }

    (bf(xs) ++= pool).result()
  }
}
