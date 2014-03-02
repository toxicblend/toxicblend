package org.toxicblend.operations.zadjust.jbullet

import javax.vecmath.Vector3d
import com.bulletphysics.linearmath.VectorUtil
import com.bulletphysics.collision.dispatch.CollisionObject
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.collision.dispatch.CollisionWorld.ConvexResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalConvexResult

/**
 * Direct port of com.bulletphysics.collision.dispatch.CollisionWorld.ClosestConvexResultCallback with hitNormalWorld 
 * removed and triangleIndex added
 */
class ClosestConvexResultCallback(val fromWorld:Vector3d, val toWorld:Vector3d, val zAdjust:Double ) extends ConvexResultCallback {

  val hitPointWorld = new Vector3d();
  var hitCollisionObject: CollisionObject = null
  var triangleIndex:Int = -1
  
  @Override
  def addSingleResult(convexResult:LocalConvexResult, normalInWorldSpace:Boolean):Double = {
    // caller already does the filter on the m_closestHitFraction
    assert(convexResult.hitFraction <= closestHitFraction)

    closestHitFraction = convexResult.hitFraction
    triangleIndex = convexResult.localShapeInfo.triangleIndex
    hitCollisionObject = convexResult.hitCollisionObject
    VectorUtil.setInterpolate3(hitPointWorld, fromWorld, toWorld, closestHitFraction)
    hitPointWorld.z += zAdjust
    //hitPointWorld.set(convexResult.hitPointLocal);
    return convexResult.hitFraction;
  }
}