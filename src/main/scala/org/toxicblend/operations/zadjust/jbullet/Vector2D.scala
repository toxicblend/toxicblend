package org.toxicblend.operations.zadjust.jbullet

import javax.vecmath.Vector2d
import javax.vecmath.Tuple2d

/**
 * Quick and dirty fix for a missing rotate operation in javax.vecmath.Vector2d
 * Directly copied from toxi.geom.Vec2D
 */
class Vector2D(__x:Double, __y:Double) extends Vector2d(__x, __y){
  def this(__that:Vector2d) = this(__that.x, __that.y)
  
  def rotateThis(theta:Double) = {
    val co = Math.cos(theta)
    val si = Math.sin(theta)
    val xx = co * x - si * y
    y = si * x + co * y
    x = xx
    this
  }
  
  def addThis(that:Tuple2d) = {
    this.add(that)
    this
  }
  
  def subThis(that:Tuple2d) = {
    this.sub(that)
    this
  }
}