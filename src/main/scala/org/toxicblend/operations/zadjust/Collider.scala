package org.toxicblend.operations.zadjust

import org.toxicblend.typeconverters.ByteBufferMeshConverter
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import javax.vecmath.Vector3d
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
class Collider( val collisionWrapper:BulletFacade, val sampleStep:Double, val epsilon:Double) {
  
  val convexCallback = {
    val aabb = collisionWrapper.aabbAllModels
    val convexShape = collisionWrapper.coneShapeZ
    new ClosestConvexResultCallback(convexShape.rotation,convexShape.zAdjust,aabb.getMin.z-1, aabb.getMax.z+1)
  }
  
  val rayCallback = {
    val aabb = collisionWrapper.aabbAllModels
    new ClosestRayResultCallback(aabb.getMin.z-1, aabb.getMax.z+1)
  }
  
  def doCollisionTests(segment:IndexedSeq[Point3d]) : IndexedSeq[IndexedSeq[Point3dE]]= {
    val rv = new ArrayBuffer[IndexedSeq[Point3dE]]
    segment.sliding(2).foreach(segment => {
      val rseg = (collisionTestSegment(segment(0), segment(1)))
      rv.append(rseg)
    })
    rv
  }
  
  /**
   * iterate over each vertex pair in segments, find the corresponding vertices in levels
   * and add the two
   */
   def adjustZLevel(segments:IndexedSeq[Point3dE], levels:IndexedSeq[IndexedSeq[Point3dE]]):IndexedSeq[Point3dE] = {
     @inline
     def xyDistanceFromStart(v0:Tuple3d, v1:Tuple3d) = {
       val deltaX = v0.x-v1.x
       val deltaY = v0.y-v1.y
       math.sqrt(deltaX*deltaX+deltaY*deltaY)
     }
     
     @inline
     def adjustSample(sample:Tuple3d, interpolated:Tuple3d) = {
       sample.z = sample.z + interpolated.z
     }
     
     val direction = new Vector3dE
     val interpolated = new Vector3dE
     (0 until segments.size).sliding(2,1).foreach(segmentI =>{
       if (segmentI.size > 1){
         val fromV = segments(segmentI(0))
         val toV = segments(segmentI(1))
         direction.setSelf(toV).subSelf(fromV).normalizeSelf
         // .iterator.sliding(2).map(_.head) == iterator all but last
         levels(segmentI(0)).iterator.sliding(2).map(_.head).foreach(sample => {  
           val distance = xyDistanceFromStart(sample,fromV) 
           interpolated.setSelf(direction).scaleSelf(distance).addSelf(fromV)
           adjustSample(sample, interpolated)
         })
         // Don't interpolate the last position, it gives jagged edges. Just use the real thing
         adjustSample(levels(segmentI(0)).last, toV)
       } else {
         System.err.println("Ignoring segment with only one vertex: " + segmentI + " segments.size=" + segments.size)
       }
     })
     levels.flatten
   }
  
  def collisionTestPoint(point:Point3d):Point3dE = {
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
    new Point3dE(result.x.toFloat, result.y.toFloat, result.z.toFloat)
  }
  
  def collisionTestSegment(point1:Point3d, point2:Point3d):IndexedSeq[Point3dE] = {
    val rv = new ArrayBuffer[Point3dE]
    rv += collisionTestPoint(point1)
    rv += collisionTestPoint(point2)
    rv
  }
}