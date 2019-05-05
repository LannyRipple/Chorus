/*
 * Copyright (c) 2010-2012. Spot Influence, Inc.
 *
 * This program is only provided under the terms of a written license from Spot Influence, Inc.
 * Except where otherwise stated in the license this program is provided WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

package com.spotright.common.lib

import scala.language.implicitConversions

import scala.collection.mutable.ListBuffer

object BinPacking {

  case class Item(name: String,  size: Int)

  object Item {
    implicit def apply(kv: (String, Int)): Item = Item(kv._1, kv._2)
  }

  case class Bin(size: Int, seeds: List[Item])

  /**
   * A First-Fit Decreasing bin packing implementation.
   *
   * Sort items into decreasing order.  Iterate the resulting list and place an item into the first
   * bin it can fit into without overfilling (adding to a new bin if it doesn't fit into any
   * existing one).  Uses no more than 11/9 OPTIMAL + 1 bins.  OPTIMAL is combinatorial NP-hard.
   *
   * @param cap Maximum size of buckets.
   * @param seeds Pairs of (name -> size).
   * @param sorted Are `seeds` already in decreasing order by size?
   */
  def ffd(cap: Int, seeds: TraversableOnce[(String, Int)], sorted: Boolean = false): List[Bin] = {
    @annotation.tailrec
    def growBuckets(kv: Item, bs: List[Bin], zs: ListBuffer[Bin]): ListBuffer[Bin] = {
      val Item(_, numSeeds) = kv

      bs match {
        case Bin(bSize, elts) :: rest if (bSize + numSeeds <= cap) =>
          zs ++= Bin(bSize + numSeeds, kv :: elts) :: rest
          zs

        case Nil =>
          zs += Bin(numSeeds, List(kv))
          zs

        case y :: ys =>
          zs += y
          growBuckets(kv, ys, zs)
      }
    }

    val sortedSeeds = if (sorted) seeds.toList else seeds.toList.view.sortBy {_.swap}.reverse

    sortedSeeds.foldLeft(List.empty[Bin]) {
      (z, a) =>
        growBuckets(a, z, ListBuffer.empty[Bin]).result
    }
  }
}
