package com.spotright.chorus

import scala.util.Try

package object concurrent {

  type ThreadingIterator[A] = Iterator[Try[A]]

  object helpers extends ConcurrentHelpers

}
