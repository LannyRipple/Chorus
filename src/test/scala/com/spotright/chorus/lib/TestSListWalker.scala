package com.spotright.chorus.lib

import org.specs2.mutable._
import SListWalker.implicits._

class TestSListWalker extends Specification {

  "SListWalker" should {

    "walk a list" in {
      val walk = SList(1,2,3).walker

      walk.head mustEqual 1
      walk.right.head mustEqual 2
      walk.right.head mustEqual 3
    }

    "insert element into a list" in {
      SList(1,3).walker.right.insert(2).result.toString mustEqual "SList(1,2,3)"
    }

    "insert element at front of list" in {
      SList.empty[Int].walker.insert(2).result.toString mustEqual "SList(2)"
      SList(1,3).walker.insert(2).result.toString mustEqual "SList(2,1,3)"
    }

    "insert element at end of list" in {
      SList.empty[Int].walker.eol.insert(2).result.toString mustEqual "SList(2)"
      SList(1,3).walker.eol.insert(2).result.toString mustEqual "SList(1,3,2)"
    }

    "insert a list into a list" in {
      val mid = SList(2,3)

      SList(1,4).walker.right.insert(mid).result.toString mustEqual "SList(1,2,3,4)"
    }

    "insert a list into a list at the front" in {
      val mid = SList(2,3)

      SList.empty[Int].walker.insert(mid).result.toString mustEqual "SList(2,3)"
      SList(1,4).walker.insert(mid).result.toString mustEqual "SList(2,3,1,4)"
    }

    "insert a list into a list at the end" in {
      val mid = SList(2,3)

      SList.empty[Int].walker.eol.insert(mid).result.toString mustEqual "SList(2,3)"
      SList(1,4).walker.eol.insert(mid).result.toString mustEqual "SList(1,4,2,3)"
    }

    "filterInPlace" in {
      SList(1,2,3,4).filterInPlace(_ % 2 == 0).toString mustEqual "SList(2,4)"
    }

    "mapInPlace" in {
      SList(1,2,3,4).mapInPlace(_ * 2).toString mustEqual "SList(2,4,6,8)"
    }

    "merge" in {
      def left = SList(1,4,5,7,9)
      def right = SList(2,3,6,8)

      left.merge(right).toString mustEqual "SList(1,2,3,4,5,6,7,8,9)"
      right.merge(left).toString mustEqual "SList(1,2,3,4,5,6,7,8,9)"
    }
  }
}
