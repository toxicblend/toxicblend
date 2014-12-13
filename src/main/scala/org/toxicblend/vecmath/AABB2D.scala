package org.toxicblend.vecmath

import scala.IndexedSeq
import org.toxicblend.util.IntNumberUtils

class AABB2D protected (val min:Vec2D, val max:Vec2D) extends ImmutableVec2D((max.x-min.x)/2d+min.x, (max.y-min.y)/2d+min.y ) {
  //if(max.x!= )assert(max.x >= min.x)
  //assert(max.y >= min.y)
  
  protected def this() = this(Vec2D(Double.PositiveInfinity, Double.PositiveInfinity), Vec2D(Double.NegativeInfinity, Double.NegativeInfinity ) )
  protected def this(x1:Double, y1:Double, x2:Double, y2:Double) = this(Vec2D(x1,y1), Vec2D(x2,y2) )
  def center:Vec2D = this
  def containsPoint(p:Vec2D, ε:Double=Polygon2D.ε):Boolean = p.x<=max.x+ε && p.x>=min.x-ε && p.y<=max.y+ε && p.y>=min.y-ε
  def contain(other:AABB2D):Boolean = min.x <= other.min.x && min.y <= other.min.y && max.x >= other.max.x && max.y >= other.max.y

  def width = max.x-min.x
  def height = max.y-min.y
  override def toString = "AABB(" + min + "->" + max + ")"
  lazy val getTopLeft = Vec2D(min.x,max.y)
  def getTopRight = max
  def getBottomLeft = min
  lazy val getBottomRight = Vec2D(max.x,min.y)
  
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
  
  /**
   * returns true if any part of @this is inside @other or the other way around
   */
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
    if (clockwise) IndexedSeq(Vec2D(max.x, max.y), Vec2D(max.x, min.y), Vec2D(min.x, min.y), Vec2D(min.x, max.y))
    else IndexedSeq(Vec2D(max.x, max.y), Vec2D(min.x, max.y), Vec2D(min.x, min.y),Vec2D(max.x, min.y))
    
  def toPolygon2D(clockwise:Boolean=false)=Polygon2D(toIndexedSeq(clockwise))
}



object AABB2D {
  
  private class MutableAABB2DContainer(val min:MutableVec2D, val max:MutableVec2D) {
    def this() = this(MutableVec2D(Double.PositiveInfinity, Double.PositiveInfinity), MutableVec2D(Double.NegativeInfinity, Double.NegativeInfinity ) )
    
    def growToContainAABB(other:MutableAABB2DContainer) = {
      growToContainPoint(other.min)
      growToContainPoint(other.max)
      this
    }
    
    def growToContainPoint(point:Vec2D) = {
      if (point.x > max.x) max.x = point.x
      if (point.x < min.x) min.x = point.x
      if (point.y > max.y) max.y = point.y
      if (point.y < min.y) min.y = point.y
    }
  }  
  
  /**
   * a pointless parallel constructor. The non-parallel version is still faster when n=41K vertices
   */
  def parallelConstructor(vertices:IndexedSeq[Vec2D]):AABB2D = {
    
    val size = vertices.size
    val perThread = (0.5 + size.toDouble / Runtime.getRuntime().availableProcessors().toDouble).toInt

    val jobs = for (i <- 0 until size by perThread ) yield i
    
    val seqOp=(aabb:MutableAABB2DContainer, b:Int) => {
      val endElement = IntNumberUtils.min(b + perThread, size)
      for (i <- b until endElement ) 
        aabb.growToContainPoint(vertices(i))
      aabb
    }
    
    val combOp=(a:MutableAABB2DContainer,b:MutableAABB2DContainer) => a.growToContainAABB(b)
    
    val aabb = jobs.par.aggregate(new MutableAABB2DContainer) (seqOp,combOp)
    apply(aabb.min, aabb.max)
  }
  
  def apply(aabb:MutableAABB2DContainer):AABB2D = new AABB2D(Vec2D(aabb.min),Vec2D(aabb.max))
  
  def apply(vertices:Seq[Vec2D]):AABB2D = {
    val aabb = new MutableAABB2DContainer
    vertices.foreach(v => aabb.growToContainPoint(v))
    apply(aabb)
  }
  
  def apply(v1:Vec2D, v2:Vec2D):AABB2D = {
    val aabb = new MutableAABB2DContainer
    aabb.growToContainPoint(v1)
    aabb.growToContainPoint(v2)
    apply(aabb)
  }
  
  def apply(x1:Double, y1:Double, x2:Double, y2:Double) = {
    val (xmin, xmax) = if(x1<x2) (x1,x2) else (x2,x1)
    val (ymin, ymax) = if(y1<y2) (y1,y2) else (y2,y1)
    new AABB2D(xmin,ymin,xmax,ymax)
  }
  
  def apply() = new AABB2D(Vec2D(Double.PositiveInfinity, Double.PositiveInfinity), Vec2D(Double.NegativeInfinity, Double.NegativeInfinity))
}