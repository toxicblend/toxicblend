package org.toxicblend.geometry

import scala.collection.mutable.HashSet
import toxi.geom.Vec2D
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D

/**
 * This is a 2.5D representation of a vertex
 * Distances between points are calculated in 2D while there is an additional Z parameter
 * This object also 'knows' its neighbourhood vertices
 * 
 * @param __x, __y is named this way because of the way scala handles constructor input parameters (non-var or val). 
 *     If the variable is, by accident, referenced anywhere in this class it will be retained as a 'private final' in bytecode
 */
class Vec2DZ(__x:Float, __y:Float, var z:Float, var objIndex:Int) extends Vec2D(__x,__y) {
  
  def this(v:Array[Double]) = this (v(0).toFloat, v(1).toFloat, v(2).toFloat, -1)
  def this(v:Array[Float]) = this (v(0), v(1), v(2), -1)
  def this(v:Vec2DZ) = this (v.x, v.y, v.z, v.objIndex)
  def this(v:ReadonlyVec3D) = this (v.x, v.y, v.z, -1)
  def this(v:ReadonlyVec3D,objIndex:Int) = this (v.x, v.y, v.z, objIndex)
  
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
  
  /**
   * Copy as Vec3D
   */
  def asVec3D : Vec3D = {
   new Vec3D(x,y,z)
  }
}