package org.toxicblend.geometry

import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.Plane
import toxi.geom.Ray3D
import toxi.geom.Triangle3D
import scala.collection.mutable.ArrayBuffer

/**
 * result container for the triangle-plane intersection calculations
 * forwardPoint is the intersection in the "correct" direction
 * retroPoint is the other intersection, but in the opposite direction
 */
class TrianglePlaneIntersectionResult {
  var hasForwardPoint:Boolean = false
  val forwardPoint = new Vec3D
  var hasRetroPoint:Boolean = false
  val retroPoint = new Vec3D
  
  @inline def reset = {
    //println("unsetting retro and forward")
    hasRetroPoint = false
    hasForwardPoint = false
  }
  
  @inline def setForwardPoint(point:ReadonlyVec3D) = {
    forwardPoint.x = point.x
    forwardPoint.y = point.y
    forwardPoint.z = point.z
    hasForwardPoint = true
    //println("setting forward point: " + point)
  }
  
  @inline def unsetForwardPoint = hasForwardPoint = false
  
  @inline def setRetroPoint(point:ReadonlyVec3D) = {
    retroPoint.x = point.x
    retroPoint.y = point.y
    retroPoint.z = point.z
    hasRetroPoint = true
    //println("setting retro point: " + point)
  }
  
  @inline def unsetRetroPoint = {
    //println("unsetting retro")
    hasRetroPoint = false;
  }
 
  override def toString:String = {
    val rv = { if (hasForwardPoint) forwardPoint.toString() + " " else "no forward " } + { if (hasRetroPoint) retroPoint.toString() + " " else "no retro" }
    rv
  }
}

object TrianglePlaneIntersection {
  
  private val SLIDINGSEQUENCE = Array((0,1),(1,2),(2,0))
  val ε = 0.00001f
  
  /**
   * returns true if the two vectors point in the same direction in the XY plane
   * Note that this only works if v0 and v1 are built from the same vector, just with different scaling (+/-)
   */
  @inline 
  def isCollinearXY(v0:ReadonlyVec3D, v1:ReadonlyVec3D ):Boolean = {
    v0.x*v1.x + v0.y*v1.y > 0
  }
  
  /**
   * returns the result of: isCollinearXY(direction, samplePoint.sub(lastPosition))
   * but without creating any temporary objects
   */
  @inline 
  def isCollinearXY(direction:ReadonlyVec3D, samplePoint:ReadonlyVec3D, lastPosition:ReadonlyVec3D):Boolean = {
    direction.x*(samplePoint.x-lastPosition.x) + direction.y*(samplePoint.y-lastPosition.y) >= 0
  }
  
  /**
   * intersects a triangle with a plane. The intersection points will be returned in rvContainer
   */
  @inline
  def trianglePlaneIntersection(triangle:IndexedSeq[ReadonlyVec3D], plane:Plane, lastPosition:ReadonlyVec3D, direction:ReadonlyVec3D, rvContainer:TrianglePlaneIntersectionResult) = {
    //val forwardIntersections = new ArrayBuffer[ReadonlyVec3D](2)
    //val retroIntersections = new ArrayBuffer[ReadonlyVec3D](2)
    
    val ray = new Ray3D
    rvContainer.reset
    SLIDINGSEQUENCE.foreach( i => {
      val class1 = plane.classifyPoint(triangle(i._1),ε)
      val class2 = plane.classifyPoint(triangle(i._2),ε)
      if (class1 == Plane.Classifier.ON_PLANE) {
        val point = triangle(i._1)
        if ( isCollinearXY(direction, point, lastPosition )) {
          rvContainer.setForwardPoint(point)
        } else {
          rvContainer.setRetroPoint(point)
        }
      }
      if (class2 == Plane.Classifier.ON_PLANE) {
        val point = triangle(i._2)
        if ( isCollinearXY(direction, point, lastPosition)) {
          rvContainer.setForwardPoint(point)
        } else {
          rvContainer.setRetroPoint(point)
        }
      }
      if ( class1 != Plane.Classifier.ON_PLANE && class2 != Plane.Classifier.ON_PLANE && class1 != class2)  {
        ray.set(triangle(i._1))
        ray.setDirection(triangle(i._2).sub(triangle(i._1)))
        val distance = plane.intersectRayDistance(ray)
        if (distance < 0){
          System.err.println("trianglePlaneIntersection: negative distance, debug me")
        } else {
          val point = ray.getPointAtDistance(distance)
          if (isCollinearXY(direction, point, lastPosition)) {
            rvContainer.setForwardPoint(point)
          } else {
            rvContainer.setRetroPoint(point)
          }
        }
      }
    })
  }
  
  @inline
  def segmentToZPlane(s0:ReadonlyVec3D, s1:ReadonlyVec3D):Plane = {
    // TODO: There are a ton of temporary Vec3D:s created here, and this is executed once for every vertex pair - fix it
    val fakePointOnZ = new Vec3D(s0.x, s0.y, s0.y + math.max(math.abs(s0.x-s1.x),  math.abs(s0.y-s1.y)))
    val tri = new Triangle3D(s0.copy, s1.copy, fakePointOnZ)
    new Plane(tri)
  }
  
  /**
   * finds the two points of intersection between a plane [defined by a line segment (s0 -> s1) and unlimited Z] and
   * a triangle. Puts result in the rvContainer   
   */
  @inline 
  def triangleZPlaneIntersection(s0:ReadonlyVec3D, s1:ReadonlyVec3D, triangle:IndexedSeq[ReadonlyVec3D], rvContainer:TrianglePlaneIntersectionResult) = {
    val plane = segmentToZPlane(s0,s1)
    val direction = s1.sub(s0).normalize()
    trianglePlaneIntersection(triangle, plane, s0, direction,rvContainer)
  }
  
  /**
   * Setup dst vector so that dst.x = targetXY.x and dst.y = targetXY.y
   * dst.z should be interpolated
   */
  def interpolate3D(dst:Vec3D, v0:Vec3D, v1:Vec3D, targetXY:Vec3D) = {
    val deltaX = v0.x - v1.x
    val deltaY = v0.y - v1.y
    val deltaZ = v0.z - v1.z
    val deltaTX = v0.x - targetXY.x
    val deltaTY = v0.y - targetXY.y
    dst.x = targetXY.x
    dst.y = targetXY.y
    if (deltaX==0f && deltaY==0f) {
      // doesn't matter what we pick here
      dst.z = v0.z
    } else 
      // pick the coordinate with highest numeric precision
      if (math.abs(deltaX) > math.abs(deltaY)){
        dst.z = v1.z + deltaZ*deltaTX/deltaX 
      } else {
        dst.z = v1.z + deltaZ*deltaTY/deltaY
      }
    dst
  }
}