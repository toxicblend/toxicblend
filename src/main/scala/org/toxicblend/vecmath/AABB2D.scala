package org.toxicblend.vecmath

import scala.IndexedSeq

class AABB2D protected (val min:Vec2D, val max:Vec2D) extends ImmutableVec2D((max.x-min.x)/2d+min.x, (max.y-min.y)/2d+min.y ) {
  //if(max.x!= )assert(max.x >= min.x)
  //assert(max.y >= min.y)
  
  protected def this() = this(Vec2D(Double.PositiveInfinity, Double.PositiveInfinity), Vec2D(Double.NegativeInfinity, Double.NegativeInfinity ) )
  protected def this(x1:Double, y1:Double, x2:Double, y2:Double) = this(Vec2D(x1,y1), Vec2D(x2,y2) )
  def center:Vec2D = this
  def containsPoint(p:Vec2D):Boolean = p.x <= max.x && p.x >= min.x && p.y <= max.y && p.y >= min.y
  def width = max.x-min.x
  def height = max.y-min.y
  override def toString = "AABB(" + min + "->" + max + ")"
  
  /**
   * returns a new instance that contains the sample point
   */
  def growToContainPoint(v:Vec2D):AABB2D = {
    val minx = if (v.x < min.x) v.x else min.x
    val miny = if (v.y < min.y) v.y else min.y
    val maxx = if (v.x > max.x) v.x else max.x
    val maxy = if (v.y > max.y) v.y else max.y
    new AABB2D(minx, miny, maxx, maxy)
  }
  
  def intersects(other:AABB2D):Boolean = {
    if (max.x < other.min.x) return false
    if (min.x > other.max.x) return false
    if (max.y < other.min.y) return false
    if (min.y > other.max.y) return false
    true
  }
  
  /**
   * Copy to an array of vertices
   */
  def toIndexedSeq(clockwise:Boolean=false):IndexedSeq[Vec2D] = 
    if (clockwise) IndexedSeq(Vec2D(max.x, max.y), Vec2D(min.x, max.y), Vec2D(min.x, min.y),Vec2D(max.x, min.y))
    else IndexedSeq(Vec2D(max.x, max.y), Vec2D(max.x, min.y), Vec2D(min.x, min.y), Vec2D(min.x, max.y))
    
  def toPolygon2D(clockwise:Boolean=false)=Polygon2D(toIndexedSeq(clockwise))
}

object AABB2D {
  
  @inline protected def growToContainPoint(point:Vec2D, min:MutableVec2D, max:MutableVec2D) = {
    if (point.x > max.x) max.x = point.x
    if (point.x < min.x) min.x = point.x
    if (point.y > max.y) max.y = point.y
    if (point.y < min.y) min.y = point.y
  }
  
  def apply(vertices:Seq[Vec2D]) = {
    val min = new MutableVec2D(Double.PositiveInfinity, Double.PositiveInfinity)
    val max = new MutableVec2D(Double.NegativeInfinity, Double.NegativeInfinity)
    vertices.foreach(v => growToContainPoint(v, min, max))
    new AABB2D(min,max)
  }
  
  def apply(v1:Vec2D, v2:Vec2D) = {
    val min = new MutableVec2D(Double.PositiveInfinity, Double.PositiveInfinity)
    val max = new MutableVec2D(Double.NegativeInfinity, Double.NegativeInfinity)
    growToContainPoint(v1, min, max)
    growToContainPoint(v2, min, max)
    new AABB2D(min,max)
  }
  
  def apply(x1:Double, y1:Double, x2:Double, y2:Double) = {
    val (xmin, xmax) = if(x1<x2) (x1,x2) else (x2,x1)
    val (ymin, ymax) = if(y1<y2) (y1,y2) else (y2,y1)
    new AABB2D(xmin,ymin,xmax,ymax)
  }
  
  def apply() = new AABB2D(Vec2D(Double.PositiveInfinity, Double.PositiveInfinity), Vec2D(Double.NegativeInfinity, Double.NegativeInfinity))
}