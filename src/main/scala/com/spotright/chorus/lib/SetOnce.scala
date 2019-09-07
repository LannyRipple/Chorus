package com.spotright.chorus.lib

/**
 * Create a value that can only be set once.
 *
 * This class helps with the common pattern of creating a var
 * so that it can be set after construction of the instance
 * it is an attribute of.
 */
class SetOnce[A] {
  private var _a: A = _
  def apply(a: => A): A = { if (_a == null) _a = a; _a }
  def value: A = if (_a != null) _a else throw new NullPointerException("null value")
  def get: Option[A] = Option(_a)
}
