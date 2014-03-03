package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.geometry.TrianglePlaneIntersectionResult
import toxi.geom.Vec3D
import javax.vecmath.Vector3d
import toxi.geom.ReadonlyVec3D

class TriangleCollisionResult extends TrianglePlaneIntersectionResult {
  val collisionPoint:Vec3D = new Vec3D
  var triangleIndex = -1
  
  @inline
  def setCollision(aCollisionPoint:Vector3d, aTriangleIndex:Int) = {
    collisionPoint.x = aCollisionPoint.x.toFloat
    collisionPoint.y = aCollisionPoint.y.toFloat
    collisionPoint.z = aCollisionPoint.z.toFloat
    triangleIndex = aTriangleIndex
  }
  
  @inline
  def setCollision(aCollisionPoint:ReadonlyVec3D,aTriangleIndex:Int) = {
    collisionPoint.set(aCollisionPoint)
    triangleIndex = aTriangleIndex
  }
  
  @inline
  def setMiss(aSimulatedPoint:Vector3d) = {
    println("Setting miss at: " + aSimulatedPoint)
    collisionPoint.x = aSimulatedPoint.x.toFloat
    collisionPoint.y = aSimulatedPoint.y.toFloat
    collisionPoint.z = aSimulatedPoint.z.toFloat
    triangleIndex = -1
    hasForwardPoint = false
    hasRetroPoint = false
  }
  
  override def toString:String = collisionPoint.toString + " " + super.toString
}