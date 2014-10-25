package org.toxicblend.operations.meshgenerator.vecmath

class ImmutableVertex2D(val x:Double, val y:Double) extends Vertex2DBase {
  def this() = this(0.0,0.0)
  def interpolateTo(v:Vertex2D, f:Double):Vertex2D = new ImmutableVertex2D(x + (v.x - x) * f, y + (v.y - y) * f)
  def interpolateTo(v:Vertex2D):Vertex2D = { 
    assert(false, "dont call this") 
    new ImmutableVertex2D(x + (v.x - x) * 0d, y + (v.y - y) * 0d)
  }
}

object ImmutableVertex2D {
  def apply(x:Int, y:Int) = new ImmutableVertex2D(x,y)
  def apply(x:Double, y:Double) = new ImmutableVertex2D(x,y)
  def apply() = new ImmutableVertex2D(0d,0d)
}