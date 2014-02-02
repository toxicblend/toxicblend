package org.toxicblend.geometry
import scala.collection.mutable.HashSet
import toxi.geom.Vec2D

/**
 * This is a 2.5D representation of a vertex
 * Distances between points are calculated in 2D while there is an additional Z parameter
 * 
 * @param ẍ, ÿ is named this way because of the way scala handles constructor input parameters (non-var or val). 
 *     If the variable is, by accident, referenced anywhere in this class it will be retained as a 'private final' in bytecode
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