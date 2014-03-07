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
import com.bulletphysics.linearmath.Triangle
import com.bulletphysics.linearmath.Line3d
import com.bulletphysics.linearmath.Plane

/**
 * This is a 'per thread' object, everything that pertain to the state of a specific calculation thread should be stored here.
 * CollisionWrapper should contain the 'common to all threads' bits
 */
class Collider(val facade:BulletFacade, val sampleStep:Double, val epsilon:Double) {
  
  val directionIncrement = new Vector3dE
  var distanceToCoverSqr = 0d
  var distanceCoveredSqr = 0d
  val triangle = new Triangle
  val currentPos = new Line3d
  val currentPlane = new Plane
  val triangleData = Array(0,0,0)
  val zMin = facade.aabbAllModels.getMin.z-1
  val zMax = facade.aabbAllModels.getMax.z+1
  val interSectionResult = new Plane.IntersectionResult
  val convexCallback = new ClosestConvexResultCallback(facade.coneShapeZ.rotation, facade.coneShapeZ.zAdjust, zMin, zMax)
  val rayCallback = new ClosestRayResultCallback(zMin, zMax)
  
  /**
   * runs collision detection at the currentPos.origin point.
   * If nothing is found with raytests a convex sweeep will be performed
   */
  def testCurrentPos:HitPointWorld = {
    rayCallback.resetForReuse(currentPos.origin)
    facade.collisionWorld.rayTest(rayCallback.rayFromWorld, rayCallback.rayToWorld, rayCallback)
    // result will just be a reference to a reused variable, must be copied
    val cp = if (rayCallback.hasResult ){
      rayCallback.getResult
    } else {
      // no hit 
       
      // no hit, try convexSweep instead
      convexCallback.resetForReuse(currentPos.origin)
      facade.collisionWorld.convexSweepTest(facade.coneShapeZ.shape, convexCallback.fromT, convexCallback.toT, convexCallback)
      if (convexCallback.hasResult) {
        println("convexCallback hit at " + currentPos.origin + " " + convexCallback.hitPointWorld.point )
      }
      convexCallback.getResult
    } 
    if (cp.hasTriangleIndex) {
      facade.models(0).readTriangle(triangle, cp.triangleIndex)
      currentPlane.getZIntersectionWithTriangle(triangle,currentPos,interSectionResult)
    } else {
      interSectionResult.hasResult = false
    }
    if (!interSectionResult.hasResult) {
      println("triangle miss at " + currentPos.origin)
    }
    cp
  }

  /**
   * Collides the line defined with fromP->toP with the 'ground' 
   * Results in a sequence of points:
   *  start point (fromP)
   *  sample points where the 'ground' changes in z attitude (across edges)
   *  end of world samples where the ray misses the 'ground'
   *  end point (toP)  
   */
  def collisionTestSegment(fromP:Point3dE, toP:Point3dE, sumInputAndOutputZ:Boolean):IndexedSeq[Point3dE] = {
    @inline
    def adjustSample(sample:Tuple3d, interpolated:Tuple3d) = {
      sample.z = sample.z + interpolated.z
    }
    currentPos.setSelf(fromP,toP)
    currentPos.getZPlane(currentPlane)
    val rv = new ArrayBuffer[Point3dE]
    directionIncrement.setSelf(currentPos.dir).normalizeSelf.scaleSelf(sampleStep)
    distanceCoveredSqr = 0d
    distanceToCoverSqr = fromP.xyDistanceSqr(toP)
    val distanceToCover = math.sqrt(distanceToCoverSqr)
    
    //println("New segment" + fromP + " toP: " + toP)
    do {
      val cp = testCurrentPos
      if (interSectionResult.hasResult) {
        // we hit something    
        val prevEdgePoint = interSectionResult.prevPoint
        val nextEdgePoint = interSectionResult.nextPoint
        
        if (rv.size==0){
          rv += cp.copyHitpoint  
        } else if( rv.last.z == zMin) {
          rv += new Point3dE(prevEdgePoint); rv.last.z = zMin
          rv += new Point3dE(prevEdgePoint);
        }
        val dotP = new Vector3dE(nextEdgePoint).subSelf(fromP).normalizeSelf.xyDot(new Vector3dE(currentPos.dir).normalizeSelf)
        val distanceSqr = fromP.xyDistanceSqr(nextEdgePoint)
        if (dotP < 0) {
           println("ignoring negative dotP:" + dotP + " triangleidx:" + cp.triangleIndex )
           println(" currentPos.origin: " + currentPos.origin)
           println(" nextEdgePoint: " + nextEdgePoint)
           println(" prevEdgePoint: " + prevEdgePoint)
           println(" distanceCoveredSqr:" + distanceCoveredSqr )
           println(" distanceSqr:" + distanceSqr )
           println(" distanceToCoverSqr:" + distanceToCoverSqr )
           println(" prevPoint=" + interSectionResult.prevPoint)
           println(" dir: "+ currentPos.dir )
           println(" rv.size: "+ rv.size )
           if (rv.size == 1) {
             println(" rv(0): "+ rv(0) )
           }
           /*val junk = new Point3dE(prevEdgePoint); junk.z = zMax
           rv += junk*/
        }

        if (distanceSqr > distanceCoveredSqr && distanceSqr < distanceToCoverSqr /*&& dotP > 0*/) {
          //println("jumping ahead: " + nextEdgePoint + " distanceSqr: " + distanceSqr + " distanceCoveredSqr:" + distanceCoveredSqr+ " dotP:" + dotP)
          currentPos.origin.setSelf(nextEdgePoint)
          rv += new Point3dE(nextEdgePoint)
        } else if (distanceSqr > distanceCoveredSqr /*&& dotP > 0*/){
          // just set the overshoot position, it will be adjusted later
          currentPos.origin.setSelf(nextEdgePoint)
        } else {
          currentPos.origin.addSelf(directionIncrement)
          if(distanceSqr!=distanceCoveredSqr) {
            System.err.println("nextEdgePoint would set me back:" + nextEdgePoint + " distanceSqr: " + distanceSqr + " distanceCoveredSqr:" + distanceCoveredSqr + " dotP:" + dotP)
          }
        }
      } else {
        // we hit nothing
        if (rv.size>0 && rv.last.z == zMin && cp.point.z == zMin){
          // no need to store another air sample
        } else {
          rv += cp.copyHitpoint
        }
        currentPos.origin.addSelf(directionIncrement)
      }
      distanceCoveredSqr = fromP.xyDistanceSqr(currentPos.origin)
      //println("" + currentPos + " distanceCoveredSqr:" + distanceCoveredSqr + " distanceToCoverSqr:" + distanceToCoverSqr)
      
      // adjust the position if it overshot the target
      if (distanceCoveredSqr > distanceToCoverSqr){
        currentPos.origin.set(toP)
        rv += testCurrentPos.copyHitpoint
        distanceCoveredSqr = distanceToCoverSqr
      }
    } while (distanceCoveredSqr < distanceToCoverSqr)  
      
    if (sumInputAndOutputZ) {
      // Interpolate the Z values on a line between fromP and toP
      val interpolated = new Vector3dE
      adjustSample(rv.head, fromP)
      // iterate over each 'in the middle' point and interpolate the result
      (1 until rv.size -1).foreach(i => {
        val sample = rv(i)
        val ratio = fromP.xyDistance(sample)/distanceToCover
        VectorUtil.setInterpolate3(interpolated, fromP, toP, ratio)
        adjustSample(sample, interpolated)
      })
      // Don't interpolate the last position, it gives jagged edges. Just use the real coordinate
      adjustSample(rv.last, toP)
    }
    rv
  }
  
  def doCollisionTests(segment:IndexedSeq[Point3dE], sumZ:Boolean) : IndexedSeq[Point3dE]= {
    val rv = new ArrayBuffer[Point3dE]
    segment.sliding(2).foreach(s => {
      // temporary test
      if (s(0).x==s(1).x && s(0).y==s(1).y && s(0).z==s(1).z)
        println("ignoring test of " + s(0) + " and " + s(1) )
      else {
        rv ++= collisionTestSegment(s(0), s(1), sumZ)
      }
    })
    rv
  }
}