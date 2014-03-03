package org.toxicblend.operations.zadjust.jbullet

import javax.vecmath.Vector3d
import javax.vecmath.Point3d
import com.bulletphysics.linearmath.VectorUtil
import com.bulletphysics.collision.dispatch.CollisionObject
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.collision.dispatch.CollisionWorld.ConvexResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalConvexResult
import com.bulletphysics.linearmath.Transform

/**
 * Direct port of com.bulletphysics.collision.dispatch.CollisionWorld.ClosestConvexResultCallback with hitNormalWorld 
 * removed and triangleIndex added
 */
class ClosestConvexResultCallback(val searchstate:SearchState, val fromT:Transform, val toT:Transform, val zAdjust:Double ) extends ConvexResultCallback {
  
  def fromWorld:Vector3d = fromT.origin
  def toWorld:Vector3d = toT.origin
  
  val hitPointWorld = new Point3d();
  //var hitCollisionObject: CollisionObject = null
  //var triangleIndex:Int = -1
  
  @Override
  def addSingleResult(convexResult:LocalConvexResult, normalInWorldSpace:Boolean):Double = {
    // caller already does the filter on the m_closestHitFraction
    assert(convexResult.hitFraction <= closestHitFraction)

    closestHitFraction = convexResult.hitFraction
    //hitCollisionObject = convexResult.hitCollisionObject
    VectorUtil.setInterpolate3(hitPointWorld, fromWorld, toWorld, closestHitFraction)
    hitPointWorld.z += zAdjust

    //searchstate.currentC.setCollision(hitPointWorld, convexResult.localShapeInfo.triangleIndex)
    return closestHitFraction;
  }
  
  @inline def hasResult = closestHitFraction < 1d

  def resetForReuse(fromV:Vector3d,toV:Vector3d) = {
    fromT.origin.set(fromV)
    toT.origin.set(toV)
    //triangleIndex = -1
    closestHitFraction = 1d
    //hitCollisionObject = null
  }
}