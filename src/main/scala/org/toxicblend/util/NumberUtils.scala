package org.toxicblend.util

import math.{abs,Pi}

object NumberUtils {
  
  @inline def isAlmostEqual(a:Int, b:Int, ε:Int) = IntNumberUtils.isAlmostEqual(a,b,ε)
  @inline def isAlmostEqual(a:Double, b:Double, ε:Double) = abs(a-b) <= ε
  @inline def isAlmostEqual(a:Float, b:Float, ε:Float) = abs(a-b) <= ε
   
  /**
   * radians to degrees
   */ 
  @inline def r2d(radian:Double) = radian*180d/Pi  
   
  /** 
   * degrees to radians
   */
  @inline def d2r(radian:Double) = radian*Pi/180d  
   
  /**
   * return a tuple containing (math.min(a,b),math.max(a,b)
   */
  @inline def inAscendingOrder(a:Double,b:Double) = if (a>b) (b,a) else (a,b)
}