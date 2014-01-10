package org.toxicblend.util

object Regex {
  lazy val FLOAT_REGEX =  """(\d*(?:\.\d*)?(?:[E|e]-?\d+)?)""".r
  lazy val INT_REGEX =  """(\d+)""".r
}