package org.toxicblend.vecmath

trait Vec2D {
  def x:Double
  def y:Double
  def interpolateTo(v:Vec2D): Vec2D
  def scale(s:Double): Vec2D
  def add(xp:Double, yp:Double): Vec2D
  def add(v:Vec2D): Vec2D
  def sub(xp:Double, yp:Double): Vec2D
  def sub(v:Vec2D): Vec2D
  /**
   * almost equal operator
   */
  def =~=(v:Vec2D, p:Double): Boolean
  def normalized:Vec2D
  def magnitude:Double
  def magnitudeSquared:Double
  def distanceTo(vx:Double, vy:Double): Double
  def distanceTo(v:Vec2D): Double
  def distanceToSquared(v:Vec2D): Double
  def distanceToSquared(vx:Double, vy:Double):Double
  def heading:Double
  def interpolateTo(v:Vec2D, f:Double): Vec2D
  def toIntString:String
  def canEqual(other: Any):Boolean
  def copy:Vec2D
  def cross(v:Vec2D):Double
  def dot(v:Vec2D):Double 
}

object Vec2D {
  def apply(x:Int, y:Int) = new ImmutableVec2D(x, y)
  def apply(x:Double, y:Double) = new ImmutableVec2D(x, y)
  def apply() = new ImmutableVec2D(0d, 0d)
  def apply(angle:Double) = new ImmutableVec2D(angle)
  
  @inline def distanceToSquared(p1x:Double, p1y:Double, p2x:Double, p2y:Double) = {
    val dx = p1x - p2x
    val dy = p1y - p2y
    dx*dx + dy*dy
  }
  
  /**
   * Dot product
   */
  @inline def dot(v1x:Double, v1y:Double, v2x:Double, v2y:Double) = (v1x * v2x) + (v1y * v2y)
  
  /**
   * returns +1 if a->b->c is a counterclockwise angle
   * -1 if a->b->c is a clockwise angle
   * and 0 if a->b->c are collinear
   */
  @inline def ccw(a:Vec2D, b:Vec2D, c:Vec2D) = (b.x - a.x) * (c.y - a.y) - (c.x - a.x) * (b.y - a.y)
  
  /**
   * cross product
   */
  @inline def cross(v1x:Double, v1y:Double, v2x:Double, v2y:Double ) = (v1x * v2y) - (v1y * v2x)
  
  /**
   * cross product between (o->a) and (o->b) 
   */
  @inline def cross(o:Vec2D, a:Vec2D, b:Vec2D) = (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)
}