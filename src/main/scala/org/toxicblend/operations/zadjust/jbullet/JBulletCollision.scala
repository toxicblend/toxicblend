package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.ToxicblendException
import org.toxicblend.geometry.TrianglePlaneIntersection
import org.toxicblend.geometry.TrianglePlaneIntersectionResult
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
      collisionPoint.x = aSimulatedPoint.x.toFloat
      collisionPoint.y = aSimulatedPoint.y.toFloat
      collisionPoint.z = aSimulatedPoint.z.toFloat
      triangleIndex = -1
      hasForwardPoint = false
      hasRetroPoint = false
    }
    
    override def toString:String = collisionPoint.toString + " " + super.toString
  }
  
  /**
   * This is an effort to avoid having to create millions of Vec3D objects.
   * It's ugly, and not very scala:esqe. But it works
   */
  class SearchState(val rayFromWorld:Vector3d, val rayToWorld:Vector3d,val zMin:Double,val zMax:Double) extends RayResultCallback {
    // previousC and currentC will be swapped frequently, but no new objects will be created
    var currentC:TriangleCollisionResult = new TriangleCollisionResult
    var hasPrevious:Boolean = false
    var previousC:TriangleCollisionResult = new TriangleCollisionResult
    val segmentFromV:Vec3D = new Vec3D
    val segmentToV:Vec3D = new Vec3D
    var distanceToCoverSqr = 0d
    var distanceCoveredSqr = 0d
    var plane:Plane = new Plane
    var directionDelta:Vec3D = new Vec3D
    var directionNormalized:Vec3D = new Vec3D
    val hitPointWorld = new Vector3d
    var endOfSegmentReached = false
    val convexCallback = new ClosestConvexResultCallback(rayFromWorld, rayToWorld, collisionWrapper.coneShapeZ.zAdjust)
    val fromTransform = new Transform(rayFromWorld,collisionWrapper.coneShapeZ.rotation)
    val toTransform = new Transform(rayToWorld,collisionWrapper.coneShapeZ.rotation)
      
    /**
     * callback from jbullet on collision
     */
    override def addSingleResult(rayResult:LocalRayResult, normalInWorldSpace:Boolean):Double = {
      closestHitFraction = rayResult.hitFraction      
      VectorUtil.setInterpolate3(hitPointWorld, rayFromWorld, rayToWorld, closestHitFraction)
      currentC.setCollision(hitPointWorld, rayResult.localShapeInfo.triangleIndex)
      val triangle = models(0).getFaces(currentC.triangleIndex).toIndexedSeq.map(i => models(0).getVertices(i))
      TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, currentC.collisionPoint, directionNormalized, currentC)
      closestHitFraction
    }
      
    @inline
    def swapStates = {
      hasPrevious = true
      val tmp = previousC
      previousC = currentC
      currentC = tmp
      currentC.reset
    }
    
    def doExtraConvexSweepTest(collisionWrapper:CollisionWrapper) = {
      println("Something is fishy at " + rayToWorld)
      fromTransform.origin.set(rayFromWorld)
      toTransform.origin.set(rayToWorld)
      convexCallback.closestHitFraction = 1d
      collisionWrapper.collisionWorld.convexSweepTest(collisionWrapper.coneShapeZ.shape, fromTransform, toTransform, convexCallback);
      if (convexCallback.closestHitFraction < 1d) {
        println("convexCallback has hit at "  + convexCallback.hitPointWorld + " triangle index = " + convexCallback.triangleIndex)
        val triangle = models(0).getFaces(convexCallback.triangleIndex).toIndexedSeq.map(i => models(0).getVertices(i))
        currentC.setCollision(hitPointWorld, convexCallback.triangleIndex)
        TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, currentC.collisionPoint, directionNormalized, currentC)
        if (currentC.hasRetroPoint) {
           println("collisionState.retroPoint "  + currentC.retroPoint) 
        } else {
          println("collisionState missing retroPoint") 
        }
        if (currentC.hasForwardPoint) {
           println("collisionState.forwardPoint "  + currentC.forwardPoint) 
        } else {
          println("collisionState missing forwardPoint") 
        }
      } 
      convexCallback.closestHitFraction
    }
      
    def collisionTest:Boolean = {  
      closestHitFraction = 1f
      //println("testing " + rayFromWorld + " to " + rayToWorld)
      collisionWrapper.collisionWorld.rayTest(rayFromWorld,rayToWorld,this)
      if ( closestHitFraction >= 1d ) {
        // raytest missed, try with convexSweepTest
        if (hasPrevious && previousC.collisionPoint.z > rayToWorld.z) {
          closestHitFraction = doExtraConvexSweepTest(collisionWrapper)
        } else {
          // we hit nothing, mark 'end of ray' as collision point 
          //println("convexCallback had no hit")
          currentC.setMiss(rayToWorld)
          //println("rayTest " + rayFromWorld + " -> " + rayToWorld + " -> air " + currentC.collisionPoint)
        }
      } else {
        val i=0 // breakpoint
      }
      
      var returnValue = false
      if ( closestHitFraction < 1d && currentC.hasForwardPoint ) {
        val distanceToTriangleIntersection = JBulletUtil.sqrXYDistance(segmentFromV,currentC.forwardPoint)
        // test if the forward point is actually behind us (can happen with convex sweep) 
        if ( distanceToTriangleIntersection < distanceCoveredSqr) {
          currentC.setMiss(rayToWorld)
        } else {
          returnValue = true
        
          if ( distanceToTriangleIntersection > distanceToCoverSqr ) {
            // The forward point was too far ahead
            
            // sample at segment end
            conditionalJumpAheadToXY(segmentToV)
            closestHitFraction = 1f 
            val oldX = currentC.collisionPoint.x
            val oldY = currentC.collisionPoint.y
            val oldZ = currentC.collisionPoint.z
            
            collisionWrapper.collisionWorld.rayTest(rayFromWorld,rayToWorld,this)
            
            currentC.forwardPoint.x = currentC.collisionPoint.x
            currentC.forwardPoint.y = currentC.collisionPoint.y
            currentC.forwardPoint.z = currentC.collisionPoint.z
            currentC.collisionPoint.x = oldX
            currentC.collisionPoint.y = oldY
            currentC.collisionPoint.z = oldZ
            currentC.hasForwardPoint = true
            //println("forward point overshot " + oldStr + " interpolated to " + currentC.forwardPoint.x + ", " + currentC.forwardPoint.y+ ", " + currentC.forwardPoint.z)
            returnValue = true
          }
        }
      }
      returnValue
    }
    
    /**
     * resets the instance for reuse  
     */  
    def setSegment(newFromV:ReadonlyVec3D, newToV:ReadonlyVec3D) = {
      currentC.hasForwardPoint = false
      currentC.hasRetroPoint = false
      previousC.hasForwardPoint = false
      previousC.hasRetroPoint = false
      hasPrevious = false
      closestHitFraction = 1f
      endOfSegmentReached = false
      
      segmentFromV.x = newFromV.x; segmentFromV.y = newFromV.y; segmentFromV.z = newFromV.z;
      segmentToV.x = newToV.x; segmentToV.y = newToV.y; segmentToV.z = newToV.z;

      distanceToCoverSqr = JBulletUtil.sqrXYDistance(segmentFromV,segmentToV)
      distanceCoveredSqr = 0d
      plane = TrianglePlaneIntersection.segmentToZPlane(segmentFromV,segmentToV)
      directionNormalized = {
        val d = segmentToV.sub(segmentFromV)
        d.z = 0
        d.normalize
      } 
      directionDelta = directionNormalized.scale(sampleDelta)
      
      setRayOrigin(segmentFromV)
      //println("setSegment: From= "+ fromV.x + "," + fromV.y + " to=" + toV.x + "," + toV.y + " Direction = " + direction.x + "," + direction.y)
    }
    
    @inline 
    def incrementPosition = {
      //val oldStr = "" + rayFromWorld.x + "," + rayFromWorld.y + " "
      //val atSegmentEnd = math.abs(rayFromWorld.x-toV.x) <= ε && math.abs(rayFromWorld.y-toV.y) <= ε
      //println(" toV.x         =" +  toV.x +         "  toV.y=        " + toV.y)
      //println(" rayFromWorld.x=" + rayFromWorld.x + " rayFromWorld.y=" + rayFromWorld.y)
      
      rayFromWorld.x += directionDelta.x
      rayFromWorld.y += directionDelta.y
      rayToWorld.x = rayFromWorld.x
      rayToWorld.y = rayFromWorld.y
      
      distanceCoveredSqr = JBulletUtil.sqrXYDistance(segmentFromV,rayFromWorld) 
      //println("distanceCoveredSoFar=" + distanceCoveredSoFar + " distanceToCoverSqr=" + distanceToCoverSqr + " :" + (distanceCoveredSoFar > distanceToCoverSqr)  + " atSegmentEnd:" + atSegmentEnd + " endOfSegmentReached:" + endOfSegmentReached)

      if (distanceCoveredSqr > distanceToCoverSqr){
        //if (!atSegmentEnd){
          //println("went to far, testing segment end")
          rayFromWorld.x = segmentToV.x
          rayFromWorld.y = segmentToV.y
          rayToWorld.x = segmentToV.x
          rayToWorld.y = segmentToV.y
          distanceCoveredSqr = distanceToCoverSqr
        //}
      }
      //println("Incrementing position from " + oldStr + " to: " + rayFromWorld.x + "," + rayFromWorld.y + " should stop at " +toV.x + "," + toV.y )
    }
    
    /**
     * jumps the 'scan' point, but if the new coordinate exceeds the current segment it will be clipped to toV
     * The method only touches X & Y coordinates
     */
    def conditionalJumpAheadToXY(point:Vec3D) = {
      val newDistanceSquared = JBulletUtil.sqrXYDistance(segmentFromV,point)
      if (newDistanceSquared > distanceToCoverSqr){
        rayFromWorld.x = segmentToV.x
        rayFromWorld.y = segmentToV.y
        rayToWorld.x = segmentToV.x
        rayToWorld.y = segmentToV.y
        distanceCoveredSqr = distanceToCoverSqr
        //println("jumping ahead to " + rayFromWorld + " (limited)")
      } else {
        rayFromWorld.x = point.x
        rayFromWorld.y = point.y
        rayToWorld.x = point.x
        rayToWorld.y = point.y
        distanceCoveredSqr = newDistanceSquared
        //println("jumping ahead to " + rayFromWorld)
      }
    }
    
    /**
     * tests if we have reached the end of the segment. 
     * But allow to sample at the end coordinate for one iteration
     */
    @inline
    def isDone = {
      if (endOfSegmentReached) {
        // end of segment already sampled, we're done
        //println("isDone: endOfSegmentReached already seen, ending iteration at:" + rayFromWorld)
        true
      } else {
        //val diff = math.abs(distanceCoveredSoFar-distanceToCoverSqr)
        if ( distanceCoveredSqr >= distanceToCoverSqr) {
          // allow one more iteration at 'end of segment'
          endOfSegmentReached = true
          //println("isDone: setting endOfSegmentReached at: " + rayFromWorld + " distanceCoveredSqr=" + distanceCoveredSqr)
        }
        //println("isDone: continuing, distanceCoveredSqr = " + distanceCoveredSqr)
        false
      }
    }
    
    @inline
    def setRayOrigin(origin:ReadonlyVec3D) {
      rayFromWorld.x=origin.x
      rayFromWorld.y=origin.y
      rayFromWorld.z=zMax
      
      rayToWorld.x=origin.x
      rayToWorld.y=origin.y
      rayToWorld.z=zMin
    }
  }
  
  val collisionWrapper = new CollisionWrapper(models)
   
   def doCollisionTests(segments:IndexedSeq[ReadonlyVec3D]):IndexedSeq[IndexedSeq[Vec3D]] = {

     //val aabb =  models(0).getBounds.copy()
     //models.foreach(model => aabb.union(model.getBounds))
     
     //println("BB min:" + collisionWrapper.aabbAllModels.getMin + " max: " + collisionWrapper.aabbAllModels.getMax + " zMin=" + collisionWrapper.zMin + " zMax=" + collisionWrapper.zMax)
     
     val searchState = new SearchState(new Vector3d(1, 1, collisionWrapper.zMax), new Vector3d(1, 1, collisionWrapper.zMin), collisionWrapper.zMin, collisionWrapper.zMax)
     val totalRayResult = new ArrayBuffer[ArrayBuffer[Vec3D]]
     
     segments.sliding(2,1).foreach(segment => {
       searchState.setSegment(segment(0), segment(1))
       
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
   
   def cleanup = {
     collisionWrapper.collisionWorld.destroy
   }
   
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

