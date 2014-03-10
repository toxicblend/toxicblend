package org.toxicblend.util

object NumberUtils {
   @inline 
   def isAlmostEqual(n1:Double, n2:Double, presicion:Double ) = {
      math.abs(n1-n2) < presicion
   }
}