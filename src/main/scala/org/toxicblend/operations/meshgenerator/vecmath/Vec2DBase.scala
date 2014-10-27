package org.toxicblend.operations.meshgenerator.vecmath

abstract class Vec2DBase extends Vec2D {
  override def toString = "("+ x + "," + y + ")"
  def toIntString = "("+ x.toInt + "," + y.toInt + ")"
  def scale(s:Double):Vec2D = if (s==1.0) this else new ImmutableVec2D(x*s, y*s)
  def add(xp:Double, yp:Double):Vec2D = if (xp==0.0 && yp==0.0) this else new ImmutableVec2D(x+xp, y+yp)
  def add(v:Vec2D):Vec2D = if (v.x==0.0 && v.y==0.0) this else new ImmutableVec2D(x+v.x, y+v.y)
  def sub(xp:Double, yp:Double):Vec2D = if (xp==0.0 && yp==0.0) this else new ImmutableVec2D(x-xp, y-yp)
  def sub(v:Vec2D):Vec2D = if (v.x==0.0 && v.y==0.0) this else new ImmutableVec2D(x-v.x, y-v.y)
  def =~=(v:Vec2D,p:Double) = (this.x - v.x).abs < p && (this.y - v.y).abs < p
  def heading = math.atan2(y, x)
  def magnitude:Double = distanceTo(0d,0d)
  def magnitudeSquared:Double = distanceToSquared(0d,0d)
  def distanceTo(vx:Double, vy:Double) = math.sqrt(distanceToSquared(vx, vy))
  def distanceTo(v:Vec2D) = math.sqrt(distanceToSquared(v))
  def distanceToSquared(v:Vec2D) = distanceToSquared(v.x, v.y)
  def distanceToSquared(vx:Double, vy:Double) = {
    val dx = x - vx
    val dy = y - vy
    dx*dx + dy*dy
  }
  
  override def hashCode = 41 * super.hashCode + x.hashCode + y.hashCode
  override def equals(other: Any) = other match {
    case that: Vec2D =>
      (that canEqual this) && this.x == that.x && this.y == that.y
    case _ =>
      false
  }
 
  def canEqual(other: Any) = other.isInstanceOf[Vec2D]
}