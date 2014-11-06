package org.toxicblend.operations.meshgenerator.vecmath

class ImmutableVec2D(val x:Double, val y:Double) extends Vec2DBase {
  def this() = this(0d,0d)
  def this(other:Vec2D) = this(other.x, other.y)
  def this(angle:Double) = this(math.cos(angle),math.sin(angle))
  
  def interpolateTo(v:Vec2D, f:Double): Vec2D = new ImmutableVec2D(x + (v.x - x) * f, y + (v.y - y) * f)
  def interpolateTo(v:Vec2D): Vec2D = { 
    assert(false, "Don't call this, this method is only here because of a compiler bug.") 
    new ImmutableVec2D()
  }
  def copy:Vec2D = new ImmutableVec2D(this)
}

object ImmutableVec2D {
  def apply(x:Int, y:Int) = new ImmutableVec2D(x, y)
  def apply(x:Double, y:Double) = new ImmutableVec2D(x, y)
  
  def apply() = new ImmutableVec2D(0d, 0d)
}