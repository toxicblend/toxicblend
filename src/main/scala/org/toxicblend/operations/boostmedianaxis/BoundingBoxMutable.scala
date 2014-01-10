package org.toxicblend.operations.boostmedianaxis

import scala.math.max
import scala.math.min
import org.toxicblend.geometry.Interval

class BoundingBoxMutableDeprecaded(var intervalX:Interval, var intervalY:Interval, var intervalZ:Interval) { 
  
  def this() = this(new Interval(Float.PositiveInfinity, Float.NegativeInfinity), 
                    new Interval(Float.PositiveInfinity, Float.NegativeInfinity), 
                    new Interval(Float.PositiveInfinity, Float.NegativeInfinity))

   // is (x, y) inside this BoundingBox?
   def contains(x:Float, y:Float):Boolean = {
     intervalX.contains(x) && intervalY.contains(y);
   }
  
   // is (x, y, z) inside this BoundingBox?
   def contains(x:Float, y:Float, z:Float):Boolean = {
     intervalX.contains(x) && intervalY.contains(y) && intervalZ.contains(z)
   } 
   
   def include(x:Float, y:Float, z:Float):Unit = {
     if (!intervalX.contains(x)){
       this.intervalX = intervalX.include(x)
     }
     if (!intervalY.contains(y)){
       this.intervalY = intervalY.include(y)
     }
     if (!intervalZ.contains(z)){
       this.intervalZ = intervalZ.include(z)
     }
   }
   
   def include(x:Array[Float]) {
     include(x(0), x(1), x(2))
   }
   
   def include(x:Array[Double]) {
     include(x(0).toFloat, x(1).toFloat, x(2).toFloat)
   }
   
   def include(x:Double, y:Double, z:Double):Unit =  {
     include(x.toFloat, y.toFloat, z.toFloat)
   }
   
   def immutable():BoundingBox = {
     new BoundingBox(intervalX,intervalY,intervalZ)
   }
   
   override def toString():String = { 
    intervalX.toString + intervalY.toString + intervalZ.toString
  }
}