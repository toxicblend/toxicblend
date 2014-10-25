package org.toxicblend.operations.meshgenerator.vecmath

abstract class Vertex2DBase extends Vertex2D {
  override def toString = {
    if (this!=null) "("+ x + "," + y + ")"
    else "null"
  }
  def toIntString = {
    if (this!=null) "("+ x.toInt + "," + y.toInt + ")"
    else "null"
  }
  def scale(s:Double):Vertex2D = if (s==1.0) this else new ImmutableVertex2D(x*s, y*s)
  def add(xp:Double, yp:Double):Vertex2D = if (xp==0.0 && yp==0.0) this else new ImmutableVertex2D(x+xp, y+yp)
  def add(v:Vertex2D):Vertex2D = if (v.x==0.0 && v.y==0.0) this else new ImmutableVertex2D(x+v.x, y+v.y)
  def =~=(v:Vertex2D,p:Double) = (this.x - v.x).abs < p && (this.y - v.y).abs < p
  @inline def distanceTo(v:Vertex2D) = math.sqrt(distanceToSquared(v))
  @inline def distanceToSquared(v:Vertex2D) = {
    val dx = x - v.x
    val dy = y - v.y
    dx*dx + dy*dy
  }
  @inline def distanceToSquared(vx:Double, vy:Double) = {
    val dx = x - vx
    val dy = y - vy
    dx*dx + dy*dy
  }

  def heading = math.atan2(y, x)
  override def hashCode = 41 * super.hashCode + x.hashCode + y.hashCode
  override def equals(other: Any) = other match {
    case that: Vertex2DBase =>
      (that canEqual this) &&
      super.equals(that) && this.x == that.x && this.y == that.y
    case _ =>
      false
  }
 
  override def canEqual(other: Any) =
    other.isInstanceOf[Vertex2D]  
}