package org.toxicblend.util

object IntNumberUtils {
  @inline def abs(a:Int) = if (a > 0) a else -a
  @inline def isAlmostEqual(a:Int, b:Int, ε:Int) = abs(a-b) <= ε
  @inline def max(a:Int, b:Int) = if (a > b) a else b
  @inline def min(a:Int, b:Int) = if (a < b) a else b
  @inline def sqrt(a:Int) = math.sqrt(a).toInt
  /**
   * return a tuple containing (math.min(a,b),math.max(a,b)
   */
  @inline def inAscendingOrder(a:Int,b:Int) = if (a>b) (b,a) else (a,b)
}