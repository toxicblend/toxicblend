package org.toxicblend.operations.boostmedianaxis

import org.toxicblend.geometry.Interval

/**
 * TODO: Why not use toxiclibs AABB??
 * @deprecated
 */
class BoundingBox(val intervalX:Interval, val intervalY:Interval, val intervalZ:Interval) { 
  
  def this() = this(new Interval(Float.PositiveInfinity, Float.NegativeInfinity), 
                    new Interval(Float.PositiveInfinity, Float.NegativeInfinity), 
                    new Interval(Float.PositiveInfinity, Float.NegativeInfinity))

   // is (x, y) inside this BoundingBox?
   def contains(x:Float, y:Float):Boolean = {
     intervalX.contains(x) && intervalY.contains(y);
   }
 
   def contains(x:Float, y:Float, z:Float):Boolean = {
     intervalX.contains(x) && intervalY.contains(y) && intervalZ.contains(z)
   } 

  override def toString():String = { 
    intervalX.toString + intervalY.toString + intervalZ.toString
  }
}