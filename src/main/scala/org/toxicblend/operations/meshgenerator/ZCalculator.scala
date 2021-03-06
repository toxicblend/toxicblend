package org.toxicblend.operations.meshgenerator

import math.{sqrt,sin,cos}

class ZCalculator {
  /**
   * find the linear interpolation parameters such that :
   * k+c*f(lowX)  = 0
   * k+c*f(highX) = 1
   */
  def interpolateParameters( f:(Double)=>Double, lowX:Double, highX:Double) = {
    val c = 1d/(f(highX)-f(lowX))
    val k = 1-c*f(highX)
    (k,c)  
  }
  
  def interpolated(f:(Double)=>Double, k:Double, c:Double, x:Double) = k+c*f(x)
  
  def calculateZ(d:Double):Double=0d
}

/**
 * Calculate Z coordinate with the formula for intersecting circles h=sqrt((4d*r*r-d*d)/4d)
 *  where d is distance between the circles and r is the radius (same for both circles)
 */
class IntersectionCalculator(val rLow:Double, val rHigh:Double) extends ZCalculator {
  
  def calculateH(r:Double, d:Double) = sqrt((4d*r*r-d*d)/4d)
  def calculateR(h:Double, d:Double) = 0.5d*sqrt(d*d+4d*h*h)
  
  // figure out linear interpolation of r:
  // r = k1+c1*d
  // when d=0 => h should be 1 and r should be 1  => k1=1
  // when d=1 => h should be hMin and r should be calculateR(hMin,1) => 
  //    c1 = (r-k1)/d == calculateR(hMin,1)-k1
  
  val hMin = rLow
  val k1 = 1d
  val c1 = calculateR(hMin,1)-k1
  val k2 = hMin/(hMin-1d)
  val c2 = 1d-k2     
  
  override def calculateZ(d:Double):Double = {
    val r = k1 + d*c1
    val h = calculateH(r,d)
    if (h.isNaN || h.isInfinite) return 0d
    val rv = k2 + c2*h
    if (rv.isNaN || rv.isInfinite) {
      println("r=" + r + " generated=" + rv + " k1=" + k1 + " c1=" + c1+ " k2=" + k2+ " k2=" + c2)
      0d
    } else rv
  }
}

/**
 * Calculate Z coordinate with the formula for circles y=sqrt(1-x*x)
 */
class ArcCalculator (val rLow:Double, val rHigh:Double) extends ZCalculator {
  
  val (k1,c1) = interpolateParameters(x=> x, rLow, rHigh)
  val (k2,c2) = interpolateParameters(x=> { val newR=(x-k1)/c1; sqrt(1-newR*newR)}, 0,1)  
  
  override def calculateZ(r:Double):Double = {
    if (c1.isNaN || c2.isNaN) 0d
    else {
      val newR = if (r>=1d) rHigh
                 else if (r<=0d) rLow
                 else (r-k1)/c1 // newR is now in the interval [rLow, rHigh]
       
      val rv = sqrt(1-newR*newR)*c2+k2
      if (rv.isNaN || rv.isInfinite) {
        //println("r=" + r + " generated " + rv + " newR=" + newR + " k1=" + k1 + " c1=" + c1+ " k2=" + k2+ " k2=" + c2)
        0d
      }
      else 1-rv
    }
  }
}
