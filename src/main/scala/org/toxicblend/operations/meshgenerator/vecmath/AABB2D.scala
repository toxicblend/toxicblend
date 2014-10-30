package org.toxicblend.operations.meshgenerator.vecmath

class AABB2D (val min:Vec2D, val max:Vec2D) extends ImmutableVec2D((max.x-min.x)/2d+min.x, (max.y-min.y)/2d+min.y ) {
  //if(max.x!= )assert(max.x >= min.x)
  //assert(max.y >= min.y)
  
  def this() = this(Vec2D(Double.PositiveInfinity, Double.PositiveInfinity), Vec2D(Double.NegativeInfinity, Double.NegativeInfinity ) )
  def this(x1:Double, y1:Double, x2:Double, y2:Double) = this(Vec2D(x1,y1), Vec2D(x2,y2) )
  def center:Vec2D = this
  def containsPoint(p:Vec2D):Boolean = p.x <= max.x && p.x >= min.x && p.y <= max.y && p.y >= min.y
  def width = max.x-min.x
  def height = max.y-min.y
  override def toString = "AABB(" + min + "->" + max + ")"
  
  def growToContainPoint(v:Vec2D):AABB2D = {
    val minx = if (v.x < min.x) v.x else min.x
    val miny = if (v.y < min.y) v.y else min.y
    val maxx = if (v.x > max.x) v.x else max.x
    val maxy = if (v.y > max.y) v.y else max.y
    new AABB2D(minx, miny, maxx, maxy)
  }
  
  /**
   * Copy to an array of vertices
   */
  def toIndexedSeq(clockwise:Boolean=false):IndexedSeq[Vec2D] = 
    if (clockwise) IndexedSeq(Vec2D(max.x, max.y), Vec2D(min.x, max.y), Vec2D(min.x, min.y),Vec2D(max.x, min.y))
    else IndexedSeq(Vec2D(max.x, max.y), Vec2D(max.x, min.y), Vec2D(min.x, min.y), Vec2D(min.x, max.y))
    
  def toPolygon2D(clockwise:Boolean=false)=new Polygon2D(toIndexedSeq(clockwise))
}  