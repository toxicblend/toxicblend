package org.toxicblend.operations.boostmedianaxis

import toxi.geom.Vec2D
import scala.collection.mutable.HashSet

/**
 * @param ẍ, ÿ is named this way because of the way scala handles constructor input parameters (non-var or val). 
 *     If the variable is, by accident, referenced anywhere in this object it will be retained as a 'private final' in bytecode
 */
class Vec2DZ(ẍ:Float,ÿ:Float,var z:Float) extends toxi.geom.Vec2D(ẍ,ÿ) {
  def this(v:Array[Double]) = this (v(0).toFloat, v(1).toFloat, v(2).toFloat)
  def this(v:Array[Float]) = this (v(0), v(1), v(2))
  def this(v:Vec2DZ) = this (v.x, v.y, v.z)
  
  var objIndex:Int = -1
  val edges = new HashSet[Vec2DZ]()
  
  def getX():Float=x
  def getY():Float=y
  def getZ():Float=z
  
  def addEdge(point:Vec2DZ):Unit={
    edges += point  
  }
  
  def addToX(xp:Float){
    //x += xp // does not work :/
    super.setComponent(0, (x+xp).toFloat)
  }
  
  def addToY(yp:Float){
    super.setComponent(1, (y+yp).toFloat)
  }
  
  def addToZ(zp:Float){
    z+=zp
  }
  
  def divideBy(xp:Float){
    //x += xp // does not work :/
    super.set(x/xp, y/xp)
    z/=xp
  }
  
  def removeEdge(point:Vec2DZ):Unit={
    edges -= point  
  }
  
  override def hashCode() = {
    AnyRef.hashCode
  }
  
  override def equals(v:Any):Boolean = {
    AnyRef.equals(v) 
  }
  
  override def toString():String = {
    "{%.4f %.4f %.4f} %d".format(getX,getY,getZ, objIndex)
  }
}