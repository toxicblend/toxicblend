package org.toxicblend.geometry

import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.Plane
import toxi.geom.Ray3D
import toxi.geom.Triangle3D
import scala.collection.mutable.ArrayBuffer

object TrianglePlaneIntersection {
  
  private val SLIDINGSEQUENCE = Array((0,1),(1,2),(2,0))
  
  /**
   * returns true if the two vectors point in the same direction in the XY plane
   */
  @inline 
  def isCollinearXY(v0:ReadonlyVec3D, v1:ReadonlyVec3D ):Boolean = {
    v0.x*v1.x + v0.y*v1.y > 0
  }
  
  /**
   * intersects a triangle with a plane. The point that is ahead of the @lastPosition in the @direction direction will be returned
   */
  @inline
  def trianglePlaneIntersection(triangle:IndexedSeq[ReadonlyVec3D], plane:Plane, lastPosition:ReadonlyVec3D, direction:ReadonlyVec3D):IndexedSeq[ReadonlyVec3D] = {
    val rv = new ArrayBuffer[ReadonlyVec3D](2)
    val epsilon = 0.00001f
    val ray = new Ray3D
    SLIDINGSEQUENCE.foreach( i => {
      val class1 = plane.classifyPoint(triangle(i._1),epsilon)
      val class2 = plane.classifyPoint(triangle(i._2),epsilon)
      if (class1 == Plane.Classifier.ON_PLANE) {
        val hit1 = triangle(i._1)
        if ( isCollinearXY(direction, hit1.sub(lastPosition) )) {
          rv.append(hit1)
        }
      } else if (class2 == Plane.Classifier.ON_PLANE) {
        val hit2 = triangle(i._1)
        if ( isCollinearXY(direction, hit2.sub(lastPosition) )) {
          rv.append(hit2)
        }
      } else if (rv.size < 2 && class1 != class2)  {
        ray.set(triangle(i._1))
        ray.setDirection(triangle(i._2).sub(triangle(i._1)))
        val distance = plane.intersectRayDistance(ray)
        if (distance < 0){
          // something went wrong
          System.err.println("negative distance")
        } else {
          val point = ray.getPointAtDistance(distance)
          if (isCollinearXY(direction, point.sub(lastPosition) )) {
            rv.append(point)
          } 
        }
      }
    })
    rv
  }
  
  @inline
  def segmentToZPlane(s0:ReadonlyVec3D, s1:ReadonlyVec3D):Plane = {
    val fakePointOnZ = new Vec3D(s0.x, s0.y, s0.y + math.max(math.abs(s0.x-s1.x),  math.abs(s0.y-s1.y)))
    val tri = new Triangle3D(new Vec3D(s0), new Vec3D(s1), fakePointOnZ)
    new Plane(tri)
  }
  
  /**
   * finds the two points of intersection between a plane [defined by a line segment (s0 -> s1) and unlimited Z] and
   * a triangle. returns empty array if no intersection was found   
   */
  @inline 
  def triangleZPlaneIntersection(s0:ReadonlyVec3D, s1:ReadonlyVec3D, triangle:IndexedSeq[ReadonlyVec3D]):IndexedSeq[ReadonlyVec3D] = {
    val plane = segmentToZPlane(s0,s1)
    val direction = s1.sub(s0).normalize()
    trianglePlaneIntersection(triangle, plane, s0, direction)
  }
}