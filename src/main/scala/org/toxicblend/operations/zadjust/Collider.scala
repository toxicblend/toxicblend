package org.toxicblend.operations.zadjust

import org.toxicblend.typeconverters.ByteBufferMeshConverter
//import javax.vecmath.Point2d
//import javax.vecmath.Point3d
//import javax.vecmath.Vector3d
import javax.vecmath.Tuple3d
import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import com.bulletphysics.linearmath.Vector3dE
import com.bulletphysics.linearmath.Point3dE
import com.bulletphysics.linearmath.VectorUtil

/**
 * This is a 'per thread' object, everything that pertain to the state of a specific calculation thread should be stored here.
 * CollisionWrapper should contain the 'common to all threads' bits
 */
class Collider(val facade:BulletFacade, val sampleStep:Double, val epsilon:Double) {
  
  val direction = new Vector3dE
  val directionIncrement = new Vector3dE
  val samplePoint = new Point3dE
  var distanceToCoverSqr = 0d
  var distanceCoveredSqr = 0d
  
  val convexCallback = {
    val aabb = facade.aabbAllModels
    val convexShape = facade.coneShapeZ
    new ClosestConvexResultCallback(convexShape.rotation,convexShape.zAdjust,aabb.getMin.z-1, aabb.getMax.z+1)
  }
  
  val rayCallback = {
    val aabb = facade.aabbAllModels
    new ClosestRayResultCallback(aabb.getMin.z-1, aabb.getMax.z+1)
  }
  
  /**
   * sample point 
   */
  def collisionTestPoint(point:Point3dE):HitPointWorld = {
    rayCallback.resetForReuse(point)
    facade.collisionWorld.rayTest(rayCallback.rayFromWorld, rayCallback.rayToWorld, rayCallback)
    // result will just be a reference to a reused variable, must be copied
    if (rayCallback.hasResult ){
      rayCallback.getResult
    } else {
      // no hit, try convexSweep instead
      convexCallback.resetForReuse(point)
      facade.collisionWorld.convexSweepTest(facade.coneShapeZ.shape, convexCallback.fromT, convexCallback.toT, convexCallback)
      //println("convexCallback hit " + convexCallback.hasResult)
      convexCallback.getResult
    }
  }

  /**
   * Collides the line defined with fromP->toP with the 'ground' 
   * Results in a sequence of points:
   *  start point (fromP)
   *  sample points where the 'ground' changes in z attitude (across edges)
   *  end of world samples where the ray misses the 'ground'
   *  end point (toP)  
   */
  def collisionTestSegment(fromP:Point3dE, toP:Point3dE, sumZ:Boolean):IndexedSeq[Point3dE] = {
     @inline
     def adjustSample(sample:Tuple3d, interpolated:Tuple3d) = {
       sample.z = sample.z + interpolated.z
     }
     
    val rv = new ArrayBuffer[Point3dE]
    direction.setSelf(toP).subSelf(fromP)
    directionIncrement.setSelf(direction).normalizeSelf.scaleSelf(sampleStep)
    samplePoint.setSelf(fromP)
    distanceCoveredSqr = 0d
    distanceToCoverSqr = fromP.xyDistance(toP)
    val distanceToCover = math.sqrt(distanceToCoverSqr)
    do {
      rv += collisionTestPoint(samplePoint).copyHitpoint
      samplePoint.addSelf(directionIncrement)
      distanceCoveredSqr = fromP.xyDistance(samplePoint)
      if (distanceCoveredSqr > distanceToCoverSqr){
        samplePoint.set(toP)
        rv += collisionTestPoint(samplePoint).copyHitpoint
        distanceCoveredSqr = distanceToCoverSqr
      }
    } while (distanceCoveredSqr < distanceToCoverSqr)  
      
    if (sumZ) {
      // Interpolate the Z values on a line between fromP and toP
      adjustSample(rv.head, fromP)
      val interpolated = new Vector3dE
      // iterator.sliding(2).map(_.head) == all but last
      //rv.tail.iterator.sliding(2).map(_.head).foreach(sample => {  
      (1 until rv.size -1).foreach(i => {
        val sample = rv(i)
        val ratio = fromP.xyDistance(sample)/distanceToCover
        VectorUtil.setInterpolate3(interpolated, fromP, toP, ratio)
        adjustSample(sample, interpolated)
      })
      // Don't interpolate the last position, it gives jagged edges. Just use the real thing
      adjustSample(rv.last, toP)
    }
    rv
  }
  
  def doCollisionTests(segment:IndexedSeq[Point3dE], sumZ:Boolean) : IndexedSeq[Point3dE]= {
    val rv = new ArrayBuffer[Point3dE]
    segment.sliding(2).foreach(s => rv ++= collisionTestSegment(s(0), s(1), sumZ))
    rv
  }
}