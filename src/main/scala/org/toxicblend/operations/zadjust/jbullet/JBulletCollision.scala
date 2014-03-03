package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.ToxicblendException
import org.toxicblend.geometry.TrianglePlaneIntersection
import com.bulletphysics.util.ObjectArrayList
import com.bulletphysics.collision.broadphase.BroadphaseInterface
import com.bulletphysics.collision.broadphase.AxisSweep3_32
import com.bulletphysics.collision.broadphase.DbvtBroadphase
import com.bulletphysics.collision.shapes.BvhTriangleMeshShape
import com.bulletphysics.collision.shapes.CollisionShape
import com.bulletphysics.collision.shapes.TriangleIndexVertexArray
import com.bulletphysics.collision.shapes.ConeShapeZ
import com.bulletphysics.collision.shapes.ConvexShape
import com.bulletphysics.collision.shapes.UniformScalingShape
import com.bulletphysics.collision.dispatch.DefaultCollisionConfiguration
import com.bulletphysics.collision.dispatch.CollisionDispatcher
import com.bulletphysics.collision.dispatch.CollisionFlags
import com.bulletphysics.collision.dispatch.CollisionWorld
import com.bulletphysics.collision.dispatch.CollisionObject
import com.bulletphysics.collision.dispatch.CollisionWorld.RayResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.linearmath.DefaultMotionState
import com.bulletphysics.linearmath.VectorUtil
import org.toxicblend.typeconverters.Mesh3DConverter
import scala.collection.mutable.ArrayBuffer
import com.bulletphysics.linearmath.QuaternionUtil
import com.bulletphysics.linearmath.Transform
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3d
import toxi.geom.Vec3D
import javax.vecmath.Quat4d
import toxi.geom.ReadonlyVec3D
import toxi.geom.Plane
import toxi.geom.Ray3D
import toxi.geom.AABB

class JBulletCollision(val models:IndexedSeq[Mesh3DConverter], val sampleDelta:Float, val ε:Float) {
   
  val collisionWrapper = new CollisionWrapper(models)
   
   def doCollisionTests(segments:IndexedSeq[ReadonlyVec3D]):IndexedSeq[IndexedSeq[Vec3D]] = {

     //val aabb =  models(0).getBounds.copy()
     //models.foreach(model => aabb.union(model.getBounds))
     
     //println("BB min:" + collisionWrapper.aabbAllModels.getMin + " max: " + collisionWrapper.aabbAllModels.getMax + " zMin=" + collisionWrapper.zMin + " zMax=" + collisionWrapper.zMax)
     
     val searchState = new SearchState(collisionWrapper, new Vector3d(1, 1, collisionWrapper.zMax), new Vector3d(1, 1, collisionWrapper.zMin), collisionWrapper.zMin, collisionWrapper.zMax)
     val totalRayResult = new ArrayBuffer[ArrayBuffer[Vec3D]]
     
     segments.sliding(2,1).foreach(segment => {
       searchState.setSegment(segment(0), segment(1), sampleDelta)
       
       val rayResult = new ArrayBuffer[Vec3D]
     
       @inline
       def addToRayResult(p:Vec3D) = {
         if (rayResult.size<1 || rayResult.last != p){
           rayResult.append(p)
         }
       }
       
       while (!searchState.isDone) {
         if ( searchState.collisionTest ) {
           // we hit something
           if (searchState.hasPrevious) {
             if (searchState.previousC.collisionPoint.z == collisionWrapper.zMin) {
               // we hit something but last time we didn't. So store that last miss
               val prevCopy = if (searchState.currentC.hasRetroPoint) {
                 searchState.currentC.retroPoint.copy               
               } else {
                 searchState.previousC.collisionPoint.copy
               }
               prevCopy.z = collisionWrapper.zMin.toFloat
               //println("saving (modified) air hit: " + prevCopy)
               addToRayResult(prevCopy)
               if (searchState.currentC.hasRetroPoint) 
                 addToRayResult(searchState.currentC.retroPoint.copy)
               else 
                 System.err.println("No retro point, what to do???")
             }
           } else {
             //println("saving segment first hit : " + searchState.currentC.collisionPoint)
             addToRayResult(searchState.currentC.collisionPoint.copy)
           }
           
           if (searchState.currentC.hasForwardPoint) {
             //println("Adding front edge: " + searchState.currentC.forwardPoint)
             addToRayResult(searchState.currentC.forwardPoint.copy)
             //println("jumping ahead to " + searchState.currentC.forwardPoint )
             searchState.conditionalJumpAheadToXY(searchState.currentC.forwardPoint)
           } else {
             //println("Adding plain collision point: " + searchState.currentC.collisionPoint)
             addToRayResult(searchState.currentC.collisionPoint.copy)
           }
         } else {
           // we hit nothing
           if (rayResult.size>0){
             if (rayResult.last.z != collisionWrapper.zMin){
               // we have previous result that didn't miss
               val lastHit = rayResult.last.copy
               lastHit.z = collisionWrapper.zMin.toFloat
               addToRayResult(lastHit)
             }
           } else {
             // the very first test didn't hit anything, save it as an starting point
             addToRayResult(searchState.currentC.collisionPoint.copy)
           }
         }
         searchState.incrementPosition
         searchState.swapStates
       }
       // end of segment
       if (rayResult.size == 0 || 
            (searchState.hasPrevious && 
              (JBulletUtil.sqrXYDistance(searchState.segmentToV,rayResult.last) > 
               JBulletUtil.sqrXYDistance(searchState.segmentToV,searchState.previousC.collisionPoint)))) {
         // segment completed, save the last of the hits points 
         //println("done all tests, saving last hit" + searchState.previousC.collisionPoint)
         addToRayResult(searchState.previousC.collisionPoint.copy)
       }
       totalRayResult.append(rayResult)
     })
     totalRayResult
   }
   
   def cleanup = collisionWrapper.collisionWorld.destroy
   
   /**
    * iterate over each vertice pair in segments, find the corresponding vertices in levels
    * and add the two
    */
   def adjustZLevel(segments:IndexedSeq[ReadonlyVec3D], levels:IndexedSeq[IndexedSeq[Vec3D]]):IndexedSeq[ReadonlyVec3D] = {
     
     def xyDistanceFromStart(v0:ReadonlyVec3D, v1:ReadonlyVec3D):Float = {
       val deltaX = v0.x-v1.x
       val deltaY = v0.y-v1.y
       math.sqrt(deltaX*deltaX+deltaY*deltaY).toFloat
     }
     
     @inline
     def adjustSample(sample:Vec3D, interpolated:ReadonlyVec3D) = {
       sample.z = sample.z + interpolated.z
     }

     (0 until segments.size).sliding(2,1).foreach(segmentI =>{
       if (segmentI.size > 1){
         val fromV = segments(segmentI(0))
         val toV = segments(segmentI(1))
         val direction = toV.sub(fromV).normalize
         levels(segmentI(0)).iterator.sliding(2).map(_.head).foreach(sample => {  // .iterator.sliding(2).map(_.head) == iterator all but last
           val distance = xyDistanceFromStart(sample,fromV) 
           val interpolated = fromV.add(direction.scale(distance))
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
}

