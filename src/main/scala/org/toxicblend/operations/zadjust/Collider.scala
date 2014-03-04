package org.toxicblend.operations.zadjust

import org.toxicblend.typeconverters.ByteBufferMeshConverter
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec3D

/**
 * This is a 'per thread' object, everything that pertain to the state of a specific calculation thread should be stored here.
 * CollisionWrapper should contain the 'common to all threads' bits
 */
class Collider( val collisionWrapper:CollisionWrapper, val sampleStep:Double, val epsilon:Double) {
  
  val convexCallback = {
    val aabb = collisionWrapper.aabbAllModels
    val convexShape = collisionWrapper.coneShapeZ
    new ClosestConvexResultCallback(convexShape.rotation,convexShape.zAdjust,aabb.getMin.z-1, aabb.getMax.z+1)
  }
  
  val rayCallback = {
    val aabb = collisionWrapper.aabbAllModels
    new ClosestRayResultCallback(aabb.getMin.z-1, aabb.getMax.z+1)
  }
  
  def doCollisionTests(segment:IndexedSeq[Point3d]) : IndexedSeq[Vec3D]= {
    val rv = new ArrayBuffer[Vec3D]
    segment.sliding(2).foreach(segment => {
      val rseg = (collisionTestSegment(segment(0), segment(1)))
      rv ++= rseg
    })
    rv
  }
  
  /**
   * iterate over each vertex pair in segments, find the corresponding vertices in levels
   * and add the two
   */
  def adjustZLevel(segments:IndexedSeq[Point3d], levels:IndexedSeq[Vec3D]):IndexedSeq[Vec3D] = {
    new ArrayBuffer[Vec3D]
  }
   
  def collisionTestPoint(point:Point3d):Vec3D = {
    rayCallback.resetForReuse(point)
    collisionWrapper.collisionWorld.rayTest(rayCallback.rayFromWorld, rayCallback.rayToWorld, rayCallback)
    val result = if (rayCallback.hasResult ){
      rayCallback.getResult
    } else {
      convexCallback.resetForReuse(point)
      collisionWrapper.collisionWorld.convexSweepTest(collisionWrapper.coneShapeZ.shape, convexCallback.fromT, convexCallback.toT, convexCallback)
      //println("convexCallback hit " + convexCallback.hasResult)
      convexCallback.getResult
    }
    new Vec3D(result.x.toFloat, result.y.toFloat, result.z.toFloat)
  }
  
  def collisionTestSegment(point1:Point3d, point2:Point3d):IndexedSeq[Vec3D] = {
    val rv = new ArrayBuffer[Vec3D]
    rv += collisionTestPoint(point1)
    rv += collisionTestPoint(point2)
    rv
  }
}