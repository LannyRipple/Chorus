package com.spotright.chorus.lib

import org.specs2.mutable._

class TestSList extends Specification {

  "SList" should {

    "be empty as constructed" in {
      new SList[Int].isEmpty must beTrue
    }

    "have a working length" in {
      var list = new SList[Int]
      list.length mustEqual 0

      list = new SList(2, list)
      list.length mustEqual 1

      list = new SList(1, list)
      list.length mustEqual 2
    }

    "have a working toString" in {
      var list = new SList[Int]
      list = new SList(2, list)
      list = new SList(1, list)

      list.toString mustEqual "SList(1,2)"
    }

    "have a working companion apply" in {
      SList.empty[Int].isEmpty must beTrue

      SList[Int]().isEmpty must beTrue
      SList(1).length mustEqual 1
      SList(1, 2).length mustEqual 2

      SList(List(1,2).iterator).length mustEqual 2
    }
  }
}
