package org.toxicblend.operations.meshgenerator.vecmath

class MutableVec2D(var x:Double, var y:Double) extends Vec2DBase {
  
  def this() = this(0d,0d)
  override def interpolateTo(v:Vec2D, f:Double): Vec2D = new MutableVec2D(x + (v.x - x) * f, y + (v.y - y) * f)
  
  def interpolateTo(v:Vec2D):Vec2D = { 
    assert(false, "dont call this, this method is only here because of a bug in the compiler") 
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
}

object MutableVec2D {
  //def apply(x:Int, y:Int) = new ImmutableVertex2D(x,y)
  //def apply(x:Double, y:Double) = new ImmutableVertex2D(x,y)
  def apply() = new MutableVec2D(0d, 0d)
}