package org.toxicblend.vecmath

class MutableVec2D(var x:Double, var y:Double) extends Vec2DBase {
  
  def this() = this(0d,0d)
  def this(other:Vec2D) = this(other.x, other.y)
  
  override def interpolateTo(v:Vec2D, f:Double): Vec2D = new MutableVec2D(x + (v.x - x) * f, y + (v.y - y) * f)
  
  def interpolateTo(v:Vec2D):Vec2D = { 
    assert(false, "Don't call this, this method is only here because of a compiler bug.") 
    new ImmutableVec2D()
  }
   
  def scaleSelf(s:Double): MutableVec2D = {
    x = x*s
    y = y*s
    this
  }
  
  def addSelf(vx:Double, vy:Double): MutableVec2D = {
    x = x+vx
    y = y+vy
    this
  } 
  
  def addSelf(v:Vec2D): MutableVec2D = {
    x = x+v.x
    y = y+v.y
    this
  }
  
  def subSelf(vx:Double, vy:Double): MutableVec2D = {
    x = x-vx
    y = y-vy
    this
  } 
  
  def subSelf(v:Vec2D): MutableVec2D = {
    x = x-v.x
    y = y-v.y
    this
  }
  
  def copy:Vec2D = new MutableVec2D(this)
  
  def set(v:Vec2D):MutableVec2D = {
    x = v.x
    y = v.y
    this 
  }
}

object MutableVec2D {
  def apply(x:Int, y:Int) = new MutableVec2D(x,y)
  def apply(x:Double, y:Double) = new MutableVec2D(x,y)
  def apply() = new MutableVec2D(0d, 0d)
}