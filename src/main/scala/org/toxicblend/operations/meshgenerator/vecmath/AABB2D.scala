package org.toxicblend.operations.meshgenerator.vecmath

class AABB2D (val min:Vec2D, val max:Vec2D) extends ImmutableVec2D((max.x-min.x)/2d+min.x, (max.y-min.y)/2d+min.y ) {
  def this(x1:Double, y1:Double, x2:Double, y2:Double) = this(Vec2D(x1,y1), Vec2D(x2,y2) )
  def center = this
  def containsPoint(p:Vec2D):Boolean = p.x <= max.x && p.x >= min.x && p.y <= max.y && p.y >= min.y
}