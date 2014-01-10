package org.toxicblend.geometry

import scala.math.max
import scala.math.min

class Interval(minIn:Float, maxIn:Float) { 
  
  val minRange = minIn
  val maxRange = maxIn
  
  // is x in [min, max] ?
  def contains(x:Float):Boolean = {
    minRange <= x && x <= maxRange;
  }

  // does this Interval a intersect Interval b?
  def intersects(b:Interval):Boolean = {
    if (maxRange < b.minRange) return false
    if (b.maxRange < minRange) return false
      return true
  }

  // return the length of this Interval
  def length:Float = { maxRange - minRange }

  // return the smallest Interval containing this Interval and b
  def union( b:Interval) : Interval = {
    return new Interval(min(minRange, b.minRange), max(maxRange, b.maxRange));
  }
  
  // returns a new Interval or this one
  def include(x:Float):Interval = {
    if (x.isNaN() || x.isInfinity ) {
      println("Nan or junk")
    }
    if (contains(x)){
      this
    } else {
      new Interval(min(x,minRange), max(x,maxRange))
    }
  }
  
  // return string representation
  override def toString():String = {
     "[" + minRange + ", " + maxRange + "]";
  }
}