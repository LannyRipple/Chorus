package com.spotright.chorus.lib

import org.specs2.ScalaCheck
import org.specs2.mutable._

class TestAATree extends Specification with ScalaCheck {

  "AATree" should {

    "handle lots of different insert orders" ! prop { ns: List[Int] =>
      val tree = new AATree[Int](implicitly[Ordering[Int]])

      ns.forall { n =>
        tree.insert(n)
        AATree.verify(tree.tree)
      } mustEqual true

      tree.size mustEqual ns.distinct.length
    }

    "handle lots of different delete orders" ! prop { ns: List[Int] =>
      val tree = new AATree[Int](implicitly[Ordering[Int]])

      ns.foreach { n =>
        tree.insert(n)
      }

      ns.forall { n =>
        tree.delete(n)
        AATree.verify(tree.tree)
      } mustEqual true

      tree.size mustEqual 0
    }

    "merge trees" ! prop { (ns: List[Int], ms: List[Int]) =>
      val tree = new AATree[Int](implicitly[Ordering[Int]])
      val other = new AATree[Int](implicitly[Ordering[Int]])

      ns.foreach { n =>
        tree.insert(n)
      }

      ms.foreach { m =>
        other.insert(m)
      }

      tree.merge(other).iterator.toList mustEqual (ns ++ ms).sorted.distinct
    }
  }
}

