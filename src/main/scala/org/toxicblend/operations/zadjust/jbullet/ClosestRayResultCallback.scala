package org.toxicblend.operations.zadjust.jbullet

import com.bulletphysics.collision.dispatch.CollisionWorld.RayResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.linearmath.VectorUtil
import javax.vecmath.Vector3d
import org.toxicblend.geometry.TrianglePlaneIntersection

class ClosestRayResultCallback(val searchstate:SearchState, val rayFromWorld:Vector3d, val rayToWorld:Vector3d) extends RayResultCallback {
  val hitPointWorld = new Vector3d
  //var triangleIndex:Int = -1
     
  /**
   * callback from jbullet on collision
   */
  override def addSingleResult(rayResult:LocalRayResult, normalInWorldSpace:Boolean):Double = {
    closestHitFraction = rayResult.hitFraction      
    VectorUtil.setInterpolate3(hitPointWorld, rayFromWorld, rayToWorld, closestHitFraction)
    searchstate.currentC.setCollision(hitPointWorld, rayResult.localShapeInfo.triangleIndex)
    val triangle = searchstate.collisionWrapper.models(0).getFaces(searchstate.currentC.triangleIndex).toIndexedSeq.map(i => searchstate.collisionWrapper.models(0).getVertices(i))
    TrianglePlaneIntersection.trianglePlaneIntersection(triangle, searchstate.segmentPlane, searchstate.currentC.collisionPoint, searchstate.directionNormalized, searchstate.currentC)
    closestHitFraction
  }
  
  @inline def hasResult = closestHitFraction < 1d
  
  def resetForReuse = {
    //triangleIndex = -1
    closestHitFraction = 1d
  }
}