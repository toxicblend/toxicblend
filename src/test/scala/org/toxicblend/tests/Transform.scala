package org.toxicblend.tests

class Transform(val scaleX:Float, val scaleY:Float, val scaleZ:Float, val offsetX:Float, val offsetY:Float, val offsetZ:Float) {
  
  def transformX(x:Float):Float = {
    scaleX*(x+offsetX)
  }
  
  def transformY(x:Float):Float = {
    scaleX*(x+offsetX)
  }
  
  def transformZ(x:Float):Float = {
    scaleX*(x+offsetX)
  }
}