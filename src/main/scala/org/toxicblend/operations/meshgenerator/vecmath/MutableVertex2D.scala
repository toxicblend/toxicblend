package org.toxicblend.operations.meshgenerator.vecmath

class MutableVertex2D(var x:Double, var y:Double) extends Vertex2DBase {
  def this() = this(0.0,0.0)
  override def interpolateTo(v:Vertex2D, f:Double):Vertex2D = new MutableVertex2D(x + (v.x - x) * f, y + (v.y - y) * f)
  
  def interpolateTo(v:Vertex2D):Vertex2D = { 
    assert(false, "dont call this") 
    new ImmutableVertex2D(x + (v.x - x) * 0d, y + (v.y - y) * 0d)
  }
   
  def scaleSelf(s:Double):MutableVertex2D = {
    x = x*s
    y = y*s
    this
  }
  
  def addSelf(vx:Double, vy:Double):MutableVertex2D = {
    x = x+vx
    y = y+vy
    this
  } 
  
  def addSelf(v:Vertex2D):MutableVertex2D = {
    x = x+v.x
    y = y+v.y
    this
  }
  
}

object MutableVertex2D {
  //def apply(x:Int, y:Int) = new ImmutableVertex2D(x,y)
  //def apply(x:Double, y:Double) = new ImmutableVertex2D(x,y)
  def apply() = new MutableVertex2D(0d,0d)
}