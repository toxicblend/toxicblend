package org.toxicblend.operations.zadjust

import javax.vecmath.Vector3d
import com.bulletphysics.linearmath.VectorUtil
import com.bulletphysics.collision.dispatch.CollisionObject
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.collision.dispatch.CollisionWorld.ConvexResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalConvexResult
import com.bulletphysics.linearmath.Transform
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import javax.vecmath.Matrix3d

/**
 * Direct port of com.bulletphysics.collision.dispatch.CollisionWorld.ClosestConvexResultCallback with hitNormalWorld 
 * removed and triangleIndex added
 */
class ClosestConvexResultCallback(val rot:Matrix3d, val zAdjust:Double, val minZ:Double, val maxZ:Double) extends ConvexResultCallback {
  
  val fromT = new Transform(rot); fromT.origin.z = maxZ
  val toT = new Transform(rot); toT.origin.z = minZ
  val hitPointWorld = new HitPointWorld
  //var hitCollisionObject: CollisionObject = null
    
  @Override
  def addSingleResult(convexResult:LocalConvexResult, normalInWorldSpace:Boolean):Double = {
    // caller already does the filter on the m_closestHitFraction
    assert(convexResult.hitFraction <= closestHitFraction)

    closestHitFraction = convexResult.hitFraction
    //hitCollisionObject = convexResult.hitCollisionObject
    VectorUtil.setInterpolate3(hitPointWorld.collisionPoint, fromT.origin, toT.origin, closestHitFraction)
    hitPointWorld.collisionPoint.z += zAdjust
    hitPointWorld.triangleIndex = convexResult.localShapeInfo.triangleIndex
    return convexResult.hitFraction;
  }
  
  @inline def hasResult = closestHitFraction < 1d
  
  /**
   * returns the collision result as a reference to an internal reused variable
   */
  @inline def getResult = {
    if (!hasResult) {
      hitPointWorld.collisionPoint.set(fromT.origin.x, fromT.origin.y, minZ)
    }
    // should i use the x&y from hitPointWorld or should i use the actual sample coordinate?
    hitPointWorld
  }
  
  def resetForReuse(samplePoint:Point3d) = {
    fromT.origin.x = samplePoint.x
    fromT.origin.y = samplePoint.y
    toT.origin.x = samplePoint.x
    toT.origin.y = samplePoint.y
    hitPointWorld.triangleIndex = -1
    closestHitFraction = 1d
    //hitCollisionObject = null
  }
}