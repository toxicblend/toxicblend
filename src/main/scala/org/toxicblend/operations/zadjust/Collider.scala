package org.toxicblend.operations.zadjust

import org.toxicblend.typeconverters.ByteBufferMeshConverter
import javax.vecmath.Tuple3d
import javax.vecmath.AxisAngle4d
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import toxi.geom.Vec3D
import toxi.geom.Vec2D
import toxi.geom.Triangle2D
import toxi.geom.ReadonlyVec3D
import com.bulletphysics.linearmath.Vector3dE
import com.bulletphysics.linearmath.Point3dE
import com.bulletphysics.linearmath.VectorUtil
import com.bulletphysics.linearmath.Triangle
import com.bulletphysics.linearmath.Line3d
import com.bulletphysics.linearmath.Plane
import com.bulletphysics.linearmath.Matrix4dE
import com.bulletphysics.BulletGlobals
import scala.collection.mutable.HashSet
import scala.collection.mutable.Set
import org.toxicblend.util.NumberUtils

object Collider {
  val NUMBER_OF_EXTRA_SEARCH_POINTS = 5
}

/**
 * This is a 'per thread' object, everything that pertain to the state of a specific calculation thread should be stored here.
 * Do not try to run this object from several threads, it will blow up in your face.
 * CollisionWrapper should contain the 'common to all threads' bits. Collision world, static model and such
 */
class Collider(val facade:BulletFacade, val sampleStep:Double) {
  
  class CollisionPoint {
    val pos = new Point3dE
    var distance = 0d
  }
  // Segment start point, all distances are calculated from this
  val fromP = new Point3dE
  val directionIncrement = new Vector3dE
  // distanceToCoverSqr is the squared distance between the start point to the end point in the segment we are scanning. 
  // New collision points should have distances less or equal to this
  var distanceToCoverSqr = 0d
  val triangle = new Triangle
  // Current Pos is a sample point along the segment we are sampling, valid collision points may be found ahead or behind of this point
  val currentPos = new Line3d
  // lastCollision.pos is the last point we inserted into the result array, valid collision points must be ahead of this point.
  val lastCollision = new CollisionPoint
  // currentPlane is the plane intersecting the start and end segment points, normal perpendicular to (0,0,1)
  val currentPlane = new Plane
  //val triangleData = Array(0,0,0)
  val zMin = facade.aabbAllModels.getMin.z-1
  val zMax = facade.aabbAllModels.getMax.z+1
  val intersectionResult = new Plane.IntersectionResult
  val convexCallback = new ClosestConvexResultCallback(facade.coneShapeZ.rotation, facade.coneShapeZ.zAdjust, zMin, zMax)
  val rayCallback = new ClosestRayResultCallback(zMin, zMax)
  val testedTriangles = new HashSet[Int]
  val searchAngle = new Matrix4dE().setSelf(new AxisAngle4d(Vector3dE.VECTOR001,BulletGlobals.SIMD_RADS_PER_DEG*(45d+360d/Collider.NUMBER_OF_EXTRA_SEARCH_POINTS))) 
  val triangleSearchVector = new Vector3dE
  val triangleSearchPoint = new Point3dE
   
  @inline
  def getTriangleForPos(pos:Point3dE):Boolean = {
    rayCallback.resetForReuse(pos)
    facade.collisionWorld.rayTest(rayCallback.rayFromWorld, rayCallback.rayToWorld, rayCallback)
    if (rayCallback.hasResult) {
      facade.models(0).readTriangle(triangle, rayCallback.getResult.triangleIndex)
      currentPlane.getZIntersectionWithTriangle(triangle,currentPos,fromP,intersectionResult)
      if (!intersectionResult.hasResult) {
        // how can a ray hit a triangle but that still we can't find the intersection points?
        println("This should never happpend, debug me")
        println("  pos=" + pos + " rayCallback.rayFromWorld=" + rayCallback.rayFromWorld)
        println("  tri=" + triangle)
        //val t = new Triangle2D(new Vec2D(triangle.a.x.toFloat,triangle.a.y.toFloat), 
        //                       new Vec2D(triangle.b.x.toFloat,triangle.b.y.toFloat),
        //                       new Vec2D(triangle.c.x.toFloat,triangle.c.y.toFloat))
        val i = triangle.containsXYPoint(currentPos.origin)
        def asPoint3dE(p:Tuple3d) = {
          println("new Point3dE(" + p.x + ", " + p.y + " ," + p.z + ")")
        } 
        if (i) {
          asPoint3dE(currentPos.origin)
          asPoint3dE(currentPos.dir)
          asPoint3dE(triangle.a)
          asPoint3dE(triangle.b)
          asPoint3dE(triangle.c)
        }
        println("Inside=" + i)
      }
      intersectionResult.hasResult
    } else {
      false
    }
  } 
  
  @inline
  def getTriangleForPos(pos:Point3dE, alreadyTestedTriangle:Set[Int]):Boolean = {
    rayCallback.resetForReuse(pos)
    facade.collisionWorld.rayTest(rayCallback.rayFromWorld, rayCallback.rayToWorld, rayCallback)
    if (rayCallback.hasResult) { 
      val triangleIdx = rayCallback.getResult.triangleIndex
      if (!alreadyTestedTriangle.contains(triangleIdx)) {
        facade.models(0).readTriangle(triangle, triangleIdx)
        currentPlane.getZIntersectionWithTriangle(triangle,currentPos,fromP,intersectionResult)
        if (!intersectionResult.hasResult) {
          alreadyTestedTriangle.add(triangleIdx)
        }
        intersectionResult.hasResult
      } else {
        println("Already tested triangle " + triangleIdx + " ignoring it")
        false
      }
    } else {
      false
    }
  } 
  
  /**
   * runs collision detection at the currentPos.origin point.
   * If raytests doesn't find anything a convex sweeep will be performed
   */
  def testCurrentPos:HitPointWorld = {
    if (getTriangleForPos(currentPos.origin)) {
      rayCallback.getResult
    } else {
      // no hit, try searching for the triangle 
      testedTriangles.clear
      var triangleSearchIndex = Collider.NUMBER_OF_EXTRA_SEARCH_POINTS
      triangleSearchVector.setSelf(directionIncrement).scale(0.5)
      do {
        searchAngle.transform(triangleSearchVector)
        triangleSearchPoint.setSelf(currentPos.origin).addSelf(triangleSearchVector)
        //println("Trying " + currentPos.origin + " with angleincrement:" + triangleSearchVector + " angle:" + triangleSearchVector.angle(Vector3dE.VECTOR100)*BulletGlobals.SIMD_DEGS_PER_RAD)
        if (getTriangleForPos(triangleSearchPoint, testedTriangles)) {
          // we found something
          triangleSearchIndex = -1
        }
        triangleSearchIndex-=1
      } while (triangleSearchIndex>0)
      
      if (triangleSearchIndex < 0) {
        //println("We found a triangle")
        rayCallback.getResult
      } else {
        //println("We found no triangle")
        // no hit, try convexSweep instead
        convexCallback.resetForReuse(currentPos.origin)
        facade.collisionWorld.convexSweepTest(facade.coneShapeZ.shape, convexCallback.fromT, convexCallback.toT, convexCallback)
        /*if (convexCallback.hasResult) {
          // I don't get this, my elaborate raytests didn't hit anything but the convex sweep found something
          //println("convexCallback hit at " + currentPos.origin + " " + convexCallback.hitPointWorld.point )
        } else {
          println("convexCallback miss at " + currentPos.origin + " " + convexCallback.hitPointWorld.collisionPoint )
        }*/
        val cp = convexCallback.getResult
        if (cp.hasTriangleIndex) {
          facade.models(0).readTriangle(triangle, cp.triangleIndex)
          currentPlane.getZIntersectionWithTriangle(triangle,currentPos,lastCollision.pos,intersectionResult)
        } else {
          intersectionResult.hasResult = false
        }
        cp
      }
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
  def collisionTestSegment(segmentStart:Point3dE, segmentEnd:Point3dE, sumInputAndOutputZ:Boolean):IndexedSeq[Point3dE] = {
    /**
     * Sets @lastCollision.pos to @pos and append a copy of @pos to @rv
     */
    @inline 
    def copyAndAddResultPoint(pos:Point3dE, recalculateDistance:Boolean, resultContainer:Buffer[Point3dE]) = {
      lastCollision.pos.set(pos)
      resultContainer.append(new Point3dE(pos))
      if (recalculateDistance) {
        lastCollision.distance = segmentStart.xyDistanceSqr(lastCollision.pos)
        //println("Added point: " + resultContainer.last + " new distance=" + lastCollision.distance )
      } //else {
        //println("Added point: " + resultContainer.last + " old distance=" + lastCollision.distance )
      //}
    }
    
    /**
     * Adjusts the sample to that sample.z is the sum of sample.z and interpolated.z
     */
    @inline
    def adjustSample(sample:Tuple3d, interpolated:Tuple3d) = {
      sample.z = sample.z + interpolated.z
    }
    
    this.fromP.set(segmentStart)
    currentPos.setSelf(segmentStart,segmentEnd)
    currentPos.getZPlane(currentPlane)
    val rv = new ArrayBuffer[Point3dE]
    directionIncrement.setSelf(currentPos.dir).normalizeSelf.scaleSelf(sampleStep)
    lastCollision.distance = 0d
    distanceToCoverSqr = segmentStart.xyDistanceSqr(segmentEnd)
    
    //println("New segment" + segmentStart + " toP: " + segmentEnd + " direction=" + directionIncrement)
    do {
      val cp = testCurrentPos
      if (intersectionResult.hasResult) {
        // we got a collision result     
        
        if (false){
          val dotPrev = new Vector3dE(intersectionResult.prevPoint).subSelf(segmentStart).xyDot(currentPos.dir)
          assert(math.signum(dotPrev) == math.signum(intersectionResult.prevPointSqDistance))
          val dotNext = new Vector3dE(intersectionResult.nextPoint).subSelf(segmentStart).xyDot(currentPos.dir)
          assert(math.signum(dotNext) == math.signum(intersectionResult.nextPointSqDistance))
        }
        
        if (rv.size==0){
          /*if(!NumberUtils.isAlmostEqual(cp.collisionPoint.x,segmentStart.x, BulletGlobals.SIMD_EPSILON)){
            System.err.println("wtf, cp.collisionPoint.x != fromP.x "+ cp.collisionPoint.x + "!=" +segmentStart.x )
          }
          if(!NumberUtils.isAlmostEqual(cp.collisionPoint.y,segmentStart.y, BulletGlobals.SIMD_EPSILON)){
            System.err.println("wtf, cp.collisionPoint.y != fromP.y "+ cp.collisionPoint.y + "!=" +segmentStart.y )
          }
          */
          assert(lastCollision.distance == 0d)
          copyAndAddResultPoint(cp.collisionPoint,false,rv) // no need to calculate distance it should still be 0
        } else if( rv.last.z == zMin) {
          copyAndAddResultPoint(intersectionResult.prevPoint,false, rv); rv.last.z = zMin
          copyAndAddResultPoint(intersectionResult.prevPoint,true, rv)
        }
        
        if ( intersectionResult.prevPointSqDistance > lastCollision.distance) { 
         copyAndAddResultPoint(intersectionResult.prevPoint,true, rv)
        }
        
        if ( intersectionResult.nextPointSqDistance > lastCollision.distance) { 
          if (intersectionResult.nextPointSqDistance < distanceToCoverSqr /*&& dotP > 0*/) {
            //println("jumping ahead: " + intersectionResult.nextPoint + " distanceSqr: " + intersectionResult.nextPointSqDistance + " lastCollision.distance:" + lastCollision.distance + " distanceToCoverSqr=" + distanceToCoverSqr)
            copyAndAddResultPoint(intersectionResult.nextPoint,false, rv)
            lastCollision.distance = segmentStart.xyDistanceSqr(rv.last)
            currentPos.origin.setSelf(intersectionResult.nextPoint).addSelf(directionIncrement)
          } else { 
            // distanceSqr >= distanceToCoverSqr
            
            // just set the overshoot position, it will be adjusted later
            //println("overshot to : " + intersectionResult.nextPoint + " distanceSqr: " + intersectionResult.nextPointSqDistance + " lastCollision.distance:" + lastCollision.distance + " distanceToCoverSqr=" + distanceToCoverSqr)
            currentPos.origin.setSelf(intersectionResult.nextPoint)
          }
        } else {
          currentPos.origin.addSelf(directionIncrement)
          /*if(intersectionResult.nextPointSqDistance!=lastCollision.distance) {
            System.err.println("nextEdgePoint would set me back:" + intersectionResult.nextPoint + " distanceSqr: " + intersectionResult.nextPointSqDistance + " lastCollision.distance:" + lastCollision.distance + " distanceToCoverSqr=" + distanceToCoverSqr)
          }*/
        }
      } else {
        // we hit nothing
        if (rv.size>0 && cp.collisionPoint.z == zMin) {  
          if (rv.last.z != zMin) {
            copyAndAddResultPoint(rv.last,true, rv)
            rv.last.z = zMin  // this is not the same point as above
          }
        } else {
          copyAndAddResultPoint(cp.collisionPoint,true, rv)
        }
        currentPos.origin.addSelf(directionIncrement)
      }
      //println("loop:" + currentPos + " lastCollision.distance:" + lastCollision.distance + " distanceToCoverSqr:" + distanceToCoverSqr + " " + rv.size)
      
      // adjust the position if it overshot the target
      if (segmentStart.xyDistanceSqr(currentPos.origin) >= distanceToCoverSqr) {
        //println("we overshot, adjusting to " + segmentEnd)
        currentPos.origin.set(segmentEnd)
        copyAndAddResultPoint(testCurrentPos.collisionPoint,false, rv)
        lastCollision.distance = distanceToCoverSqr
      }
    } while (lastCollision.distance < distanceToCoverSqr)  
      
    if (sumInputAndOutputZ) {
      val distanceToCover = math.sqrt(distanceToCoverSqr)
      // Interpolate the Z values on a line between fromP and toP
      val interpolated = new Vector3dE
      adjustSample(rv.head, segmentStart)
      // iterate over each 'in the middle' point and interpolate the result
      (1 until rv.size -1).foreach(i => {
        val sample = rv(i)
        val ratio = segmentStart.xyDistance(sample)/distanceToCover
        VectorUtil.setInterpolate3(interpolated, segmentStart, segmentEnd, ratio)
        adjustSample(sample, interpolated)
      })
      // Don't interpolate the last position, it gives jagged edges. Just use the real coordinate
      adjustSample(rv.last, segmentEnd)
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