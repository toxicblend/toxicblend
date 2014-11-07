package org.toxicblend.util

object NumberUtils {
   @inline 
   def isAlmostEqual(n1:Double, n2:Double, presicion:Double ) = {
      math.abs(n1-n2) < presicion
   }
   
   @inline 
   def isAlmostEqual(n1:Float, n2:Float, presicion:Float ) = {
      math.abs(n1-n2) < presicion
   }
   
   /**
    * radians to degrees
    */ 
   @inline def r2d(radian:Double) = radian*180d/math.Pi  
   
   /** 
    * degrees to radians
    */
   @inline def d2r(radian:Double) = radian*math.Pi/180d  
}