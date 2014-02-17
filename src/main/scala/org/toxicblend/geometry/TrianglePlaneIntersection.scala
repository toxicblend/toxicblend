package org.toxicblend.geometry

import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.Plane
import toxi.geom.Ray3D
import toxi.geom.Triangle3D
import scala.collection.mutable.ArrayBuffer

object TrianglePlaneIntersection {
  
  private val SLIDINGSEQUENCE = Array((0,1),(1,2),(2,0))
  @inline
  def trianglePlaneIntersection(triangle:IndexedSeq[ReadonlyVec3D], plane:Plane):IndexedSeq[ReadonlyVec3D] = {
    val rv = new ArrayBuffer[ReadonlyVec3D](2)
    val epsilon = 0.0001f
    val ray = new Ray3D
    SLIDINGSEQUENCE.foreach( i => {
      if (rv.size < 2 && plane.classifyPoint(triangle(i._1), epsilon) != plane.classifyPoint(triangle(i._2), epsilon)) {
        ray.set(triangle(i._1))
        ray.setDirection(triangle(i._2).sub(triangle(i._1)))
        val distance = plane.intersectRayDistance(ray)
        if (distance < 0){
          // something went wrong
          System.err.println("negative distance")
        } else {
          rv.append(ray.getPointAtDistance(distance))
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
    trianglePlaneIntersection(triangle,plane)
  }
}