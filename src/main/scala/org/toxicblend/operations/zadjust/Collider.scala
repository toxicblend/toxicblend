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


/**
 * This is a 'per thread' object, everything that pertain to the state of a specific calculation thread should be stored here.
 * CollisionWrapper should contain the 'common to all threads' bits
 */
class Collider(val facade:BulletFacade, val sampleStep:Double, val epsilon:Double) {
  
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
  def collisionTestPoint(point:Point3dE):Point3dE = {
    rayCallback.resetForReuse(point)
    facade.collisionWorld.rayTest(rayCallback.rayFromWorld, rayCallback.rayToWorld, rayCallback)
    // result will just be a reference to a reused variable, must be copied
    val result = if (rayCallback.hasResult ){
      rayCallback.getResult
    } else {
      convexCallback.resetForReuse(point)
      facade.collisionWorld.convexSweepTest(facade.coneShapeZ.shape, convexCallback.fromT, convexCallback.toT, convexCallback)
      //println("convexCallback hit " + convexCallback.hasResult)
      convexCallback.getResult
    }
    new Point3dE(result.x.toFloat, result.y.toFloat, result.z.toFloat)
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
    rv += collisionTestPoint(fromP)
    rv += collisionTestPoint(toP)
    if (sumZ) {
      // Interpolate the Z values on a line between fromP and toP
      val direction = new Vector3dE(toP).subSelf(fromP).normalizeSelf
      val interpolated = new Vector3dE
      // .sliding(2).map(_.head) == all but last
      rv.sliding(2).map(_.head).foreach(sample => {  
        val xyDistance = fromP.xyDistance(sample) 
        interpolated.setSelf(direction).scaleSelf(xyDistance).addSelf(fromP)
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