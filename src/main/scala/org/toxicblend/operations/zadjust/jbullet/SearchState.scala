package org.toxicblend.operations.zadjust.jbullet

import com.bulletphysics.collision.dispatch.CollisionWorld.RayResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.linearmath.VectorUtil
import javax.vecmath.Vector3d
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Plane
import com.bulletphysics.linearmath.Transform
import org.toxicblend.geometry.TrianglePlaneIntersection

/**
 * This is an effort to avoid having to create millions of Vec3D objects.
 * The intention is to let every thread running collisions to have an unique instance of this class each.
 * It's ugly, and not very scala:esqe. But it works
 */
class SearchState(val collisionWrapper:CollisionWrapper, val rayFromWorld:Vector3d, val rayToWorld:Vector3d,val zMin:Double,val zMax:Double) {
  // previousC and currentC will be swapped frequently, but no new objects will be created
  var currentC:TriangleCollisionResult = new TriangleCollisionResult
  var hasPrevious:Boolean = false
  var previousC:TriangleCollisionResult = new TriangleCollisionResult
  val segmentFromV:Vec3D = new Vec3D
  val segmentToV:Vec3D = new Vec3D
  var segmentPlane:Plane = new Plane
  var distanceToCoverSqr = 0d
  var distanceCoveredSqr = 0d
  var directionDelta:Vec3D = new Vec3D
  var directionNormalized:Vec3D = new Vec3D
  val hitPointWorld = new Vector3d
  var endOfSegmentReached = false
  val rayCallback = new ClosestRayResultCallback(this, rayFromWorld, rayToWorld)
  val convexCallback = {
    val fromTransform = new Transform(rayFromWorld,collisionWrapper.coneShapeZ.rotation)
    val toTransform = new Transform(rayToWorld,collisionWrapper.coneShapeZ.rotation)
    new ClosestConvexResultCallback(this, fromTransform, toTransform, collisionWrapper.coneShapeZ.zAdjust)
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
    //println("Something is fishy at " + rayToWorld)
    convexCallback.resetForReuse(rayFromWorld, rayToWorld)
    collisionWrapper.collisionWorld.convexSweepTest(collisionWrapper.coneShapeZ.shape, convexCallback.fromT, convexCallback.toT, convexCallback)
    if (!convexCallback.hasResult) {
      currentC.setMiss(rayToWorld)
    }
       
    if (convexCallback.hasResult && (hasPrevious && previousC.triangleIndex != currentC.triangleIndex)) {
      println("convexCallback has hit at "  + convexCallback.hitPointWorld + " triangle index = " + currentC.triangleIndex + " hasPrevious=" + hasPrevious)
      val triangle = collisionWrapper.models(0).getFaces(currentC.triangleIndex).toIndexedSeq.map(i => collisionWrapper.models(0).getVertices(i))
      
      TrianglePlaneIntersection.trianglePlaneIntersection(triangle, segmentPlane, currentC.collisionPoint, directionNormalized, currentC)
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
    } else {
     currentC.setMiss(rayToWorld)
    } 
  }
    
  def collisionTest:Boolean = {
    //println("testing " + rayFromWorld + " to " + rayToWorld)
    rayCallback.resetForReuse
    collisionWrapper.collisionWorld.rayTest(rayFromWorld,rayToWorld,rayCallback)
    if ( !rayCallback.hasResult ) {
      currentC.setMiss(rayToWorld)
      // raytest missed, try with convexSweepTest
      if (hasPrevious && previousC.collisionPoint.z > rayToWorld.z) {
        doExtraConvexSweepTest(collisionWrapper)
      } 
    } else {
      val i=0 // breakpoint
    }
    
    var returnValue = false
    if ( rayCallback.hasResult && currentC.hasForwardPoint ) {
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
          val oldX = currentC.collisionPoint.x
          val oldY = currentC.collisionPoint.y
          val oldZ = currentC.collisionPoint.z
          rayCallback.resetForReuse
          collisionWrapper.collisionWorld.rayTest(rayFromWorld,rayToWorld,rayCallback)
          
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
   * resets the instance for reuse with a new segment 
   */  
  def setSegment(newFromV:ReadonlyVec3D, newToV:ReadonlyVec3D, sampleDelta:Float) = {
    currentC.hasForwardPoint = false
    currentC.hasRetroPoint = false
    previousC.hasForwardPoint = false
    previousC.hasRetroPoint = false
    hasPrevious = false
    rayCallback.resetForReuse
    endOfSegmentReached = false
    
    segmentFromV.x = newFromV.x; segmentFromV.y = newFromV.y; segmentFromV.z = newFromV.z;
    segmentToV.x = newToV.x; segmentToV.y = newToV.y; segmentToV.z = newToV.z;

    distanceToCoverSqr = JBulletUtil.sqrXYDistance(segmentFromV,segmentToV)
    distanceCoveredSqr = 0d
    segmentPlane = TrianglePlaneIntersection.segmentToZPlane(segmentFromV,segmentToV)
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