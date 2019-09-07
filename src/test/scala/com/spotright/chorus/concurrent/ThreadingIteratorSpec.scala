package com.spotright.chorus.concurrent

import org.scalatest.Assertion

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure

class ThreadingIteratorSpec extends WordSpec with Matchers {

  val input: List[Int] = (0 until 100).toList
  val bufSize = 10
  val dwell = 250L // A sleep time longer than needed to run 2 * bufsize testing Futures

  type ThreadingIteratorFn[A] = (A => Future[A]) => ThreadingIterator[A]

  "ThreadingIterator" when {

    "executing in fifo order" should {

      def fifo: ThreadingIteratorFn[Int] = ThreadingIterator.fifo(input, bufSize)

      "keep elements in order" in {
        val iter = fifo(n => Future { n + 1 })
        val result = iter.toList

        withClue(result) {
          result.map(_.get) shouldBe input.map(_ + 1)
        }
      }

      "capture exceptions" in {
        val iter = fifo(withException)
        captureExceptions(iter)
      }

      "have no more than 2 * bufSize completed futures after start" in {
        val iterFn: ThreadingIteratorFn[Int] = ThreadingIterator.fifo(input, bufSize)
        testStart(iterFn, 20)
      }

      "should not start if start is false" in {
        val iterFn: ThreadingIteratorFn[Int] = ThreadingIterator.fifo(input, bufSize, start = false)
        testStart(iterFn, 0)
      }

      "should process even when all working Futures have completed" in {
        val iter = fifo(n => Future { n + 1 })
        testNextGuard(iter, input.length)
      }
    }

    "executing in firstAvailable order" should {

      def fa: ThreadingIteratorFn[Int] = ThreadingIterator.firstAvailable(input, bufSize)

      "return all elements" in {
        val iter = fa(n => Future { n + 1 })
        val result = iter.map(_.get).toList.sorted

        withClue(result) {
          result.size shouldBe 100
        }
      }

      "capture exceptions" in {
        val iter = fa(withException)
        captureExceptions(iter)
      }

      "have no more than 2 * bufSize completed futures after start" in {
        val iterFn: ThreadingIteratorFn[Int] = ThreadingIterator.firstAvailable(input, bufSize)
        testStart(iterFn, 20)
      }

      "should not start if start is false" in {
        val iterFn: ThreadingIteratorFn[Int] = ThreadingIterator.firstAvailable(input, bufSize, start = false)
        testStart(iterFn, 0)
      }

      "should process even when all working Futures have completed" in {
        val iter = fa(n => Future { n + 1 })
        testNextGuard(iter, input.length)
      }
    }

  }

  // Helpers

  val err = new IllegalArgumentException("42")

  def withException(n: Int): Future[Int] =
    if (n != 42) Future { n + 1 }
    else Future.failed[Int](err)

  def captureExceptions(iter: ThreadingIterator[Int]): Assertion =
    iter.find(_.isFailure).value shouldBe Failure(err)

  def testStart(iterFn: ThreadingIteratorFn[Int], expectedCount: Int): Assertion = {
    var count: Int = 0
    val iter = iterFn(n => Future { count += 1; n + 1 })

    Thread.sleep(dwell)

    (count >= expectedCount / 2) shouldBe true
    (count <= expectedCount) shouldBe true
    iter.hasNext shouldBe true // stop lint complaint about not using `iter`
  }

  def testNextGuard(iter: ThreadingIterator[Int], inputSize: Int): Assertion = {
    val first = iter.next() // start it (just in case)

    // assertion:
    // To pull the first value `working` and `ready` in the instance have been filled

    Thread.sleep(dwell)

    // assertion:
    // Slept long enough such that all the `working` have completed.  We are in a
    // situation where no further completion of Futures would cycle the process from
    //
    //    ready <- working <- input
    //
    // inside our ThreadingIterator instance.  Without the guard in next() we could
    // not complete the iteration.

    val result = (first :: iter.toList).map(_.get).sorted

    withClue(result) {
      result.length shouldBe inputSize
    }
  }

}
