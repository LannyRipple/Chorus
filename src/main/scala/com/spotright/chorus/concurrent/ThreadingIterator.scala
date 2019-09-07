package com.spotright.chorus.concurrent

import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

import scala.collection.AbstractIterator
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

object ThreadingIterator {

  /**
   * Construct an iterator that acts like Future.traverse
   *
   * $ Has no more than `size` active futures (How this differs from Future.traverse)
   * $ `size` <= (buffered Results + active Futures) <= 2 * `size`
   *
   * This datastructure is useful in Ppark for partition work that provides an
   * iterator and expects an iterator in return.
   *
   * @param in Values to be transformed via `f`
   * @param bufSize Internal buffer size(s).  Limits amount of Bs (memory) and active Futures.
   * @param duration Duration to wait for next() value.  Throws TimeoutException if exceeded.
   * @param start Should the iterator pre-buffer futures or wait to be accessed to begin processing
   * @param f Function transforming As to Bs using a Future
   */
  abstract class AbstractThreadingIterator[A, B](in: TraversableOnce[A],
                                                 bufSize: Int,
                                                 duration: Duration,
                                                 start: Boolean,
                                                 f: A => Future[B])(
    implicit ec: ExecutionContext
  ) extends AbstractIterator[Try[B]] {

    // Important that anything which is accessing this state is synchronized so as not
    // to interfere with anything else trying to access it.
    protected val iter: Iterator[Future[B]] = in.map(g(f, ec)).toIterator
    protected val working: mutable.Queue[Future[B]] = mutable.Queue.empty[Future[B]]
    protected val ready: ArrayBlockingQueue[Try[B]] = new ArrayBlockingQueue[Try[B]](bufSize, false)

    // assert:
    // The final completed Future in `working` is moved to `ready`
    // 1. By a previously completed future that is in the critical section and
    //    finds the final Future .isCompleted
    // 2. By the final Future when it completes, enters the critical section,
    //    and moves itself to .isComplete
    // 3. By the guard in next() if `ready` is empty.
    protected def makeTheDonuts(): Unit

    protected def g(f: A => Future[B], ec: ExecutionContext)(a: A): Future[B] = {
      val fx = f(a)
      fx.onComplete(_ => makeTheDonuts())(ec)
      fx
    }

    def hasNext: Boolean = synchronized {
      val hasN = iter.hasNext || working.nonEmpty || !ready.isEmpty

      // Start on access if not started
      if (working.isEmpty && hasN)
        working ++= iter.take(bufSize)

      hasN
    }

    def next(): Try[B] = synchronized {
      if (!hasNext)
        Iterator.empty.next

      // Avoid situation where all Future's completed but could not
      // offer to `ready`.  Once all `working` completed then `ready`
      // could be drained without anything to refill.
      if (ready.isEmpty)
        makeTheDonuts()

      // Modelled of scala.concurrent.impl.Promise.tryAawit()
      val item = duration match {
        case e if e eq Duration.Undefined => throw new IllegalArgumentException("cannot wait for Undefined period")
        case Duration.Inf                 => ready.poll(365L, TimeUnit.DAYS)
        case Duration.MinusInf            => ready.poll(0L, TimeUnit.MILLISECONDS)
        case f: FiniteDuration            => ready.poll(f.length, f.unit)
      }

      if (item == null) throw new TimeoutException(s"next() timed out after [$duration]")

      item
    }

    // When constructed, start `size` futures
    synchronized {
      if (start) hasNext
    }
  }

  /**
   * Construct a ThreadingIterator
   *
   * $ FIFO semantics, elements of iterator in same order as input
   */
  def fifo[A, B](in: TraversableOnce[A], bufSize: Int, duration: Duration = Duration.Inf, start: Boolean = true)(
    f: A => Future[B]
  )(implicit ec: ExecutionContext): Iterator[Try[B]] =
    new AbstractThreadingIterator[A, B](in, bufSize, duration, start, f) {

      def makeTheDonuts(): Unit = synchronized {
        // Initial condition will return ASAP if no work to do
        var capacity = ready.remainingCapacity()

        while (capacity > 0 && working.nonEmpty && working.head.isCompleted) {
          ready.add(
            // safe because future isCompleted
            working.dequeue().value.get
          )
          if (iter.hasNext) working += iter.next()
          capacity -= 1
        }
      }
    }

  /**
   * Construct a ThreadingIterator
   *
   * $ Fist-Available semantics, elements available as Futures complete
   */
  def firstAvailable[A, B](in: TraversableOnce[A],
                           bufSize: Int,
                           duration: Duration = Duration.Inf,
                           start: Boolean = true)(
    f: A => Future[B]
  )(implicit ec: ExecutionContext): Iterator[Try[B]] =
    new AbstractThreadingIterator[A, B](in, bufSize, duration, start, f) {

      def makeTheDonuts(): Unit = synchronized {
        // Initial condition will return ASAP if no work to do
        var capacity = ready.remainingCapacity()

        while (capacity > 0 && working.nonEmpty && working.exists(_.isCompleted)) {
          ready.add(
            // safe because future isCompleted
            working.dequeueFirst(_.isCompleted).flatMap(_.value).get
          )
          if (iter.hasNext) working += iter.next()
          capacity -= 1
        }
      }
    }

}
