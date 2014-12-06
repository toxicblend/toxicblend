package org.toxicblend.vecmath

abstract class Vec2DBase extends Vec2D {
  override def toString = "("+ x + "," + y + ")"
  def toIntString = "("+ x.toInt + "," + y.toInt + ")"
  def scale(s:Double):Vec2D = if (s==1.0) this else new ImmutableVec2D(x*s, y*s)
  def add(xp:Double, yp:Double):Vec2D = if (xp==0.0 && yp==0.0) this else new ImmutableVec2D(x+xp, y+yp)
  def add(v:Vec2D):Vec2D = if (v.x==0.0 && v.y==0.0) this else new ImmutableVec2D(x+v.x, y+v.y)
  def sub(xp:Double, yp:Double):Vec2D = if (xp==0.0 && yp==0.0) this else new ImmutableVec2D(x-xp, y-yp)
  def sub(v:Vec2D):Vec2D = if (v.x==0.0 && v.y==0.0) this else new ImmutableVec2D(x-v.x, y-v.y)
  def =~=(v:Vec2D,ε:Double) = Vec2D.almostEqual(this, v, ε)
  
  def normalized:Vec2D= {
    val mag = x * x + y * y
    assert(mag>0)
    val mag2 = 1d / math.sqrt(mag)
    new ImmutableVec2D(x*mag2,y*mag2)
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