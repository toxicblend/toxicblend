package org.toxicblend.tests

import toxi.geom.mesh.TriangleMesh
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Plane
import toxi.geom.Triangle3D
import org.toxicblend.typeconverters.Mesh3DConverter
import scala.collection.mutable.MutableList
import org.toxicblend.operations.zadjust.jbullet.JBulletCollision
import org.scalatest._
import org.toxicblend.geometry.TrianglePlaneIntersection
import org.toxicblend.geometry.TrianglePlaneIntersectionResult

class TrianglePlaneIntersectionTest extends FlatSpec with Matchers {
  
  val epsilon = 0.00001f
  val triangle = Array(new Vec3D(0,0,1), new Vec3D(1,0,1), new Vec3D(0,1,1))
  val result = new TrianglePlaneIntersectionResult
  
  "TrianglePlaneIntersectionTest 1" should "find intersections" in {  
    result.reset
    val lastPosition = new Vec3D(0f,1f,1f)
    val planeTriangle = new Triangle3D(new Vec3D(0f,1f,0f), new Vec3D(1f,2f,0), new Vec3D(0f,1f,1f))
    val plane = new Plane(planeTriangle)
    val direction = new Vec3D(-1f,-1f,0).normalize
    
    TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, lastPosition, direction, result)
    //println("Result: " + result)
    
    result.hasForwardPoint should be (false)
    result.hasRetroPoint should be (true)
    
    result.retroPoint.x should be ((0f) +- epsilon)
    result.retroPoint.y should be ((1f) +- epsilon)
    result.retroPoint.z should be ((1f) +- epsilon)
   
  }
  
  "TrianglePlaneIntersectionTest 2" should "find intersections crossing all 3 lines of triangle (origo+hyp)" in {  
    result.reset
    val lastPosition = new Vec3D(.2f,.2f,1f)
    val planeTriangle = new Triangle3D(new Vec3D, new Vec3D(1f,1f,0), new Vec3D(0f,0f,-1f))
    val plane = new Plane(planeTriangle)
    val direction = new Vec3D(1f,1f,0).normalize()
    
    TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, lastPosition, direction, result)
    //println("Result: " + result)
    
    result.hasForwardPoint should be (true)
    result.hasRetroPoint should be (true)
    
    result.retroPoint.x should be ((0f) +- epsilon)
    result.retroPoint.y should be ((0f) +- epsilon)
    result.retroPoint.z should be ((1f) +- epsilon)
    
    result.forwardPoint.x should be ((.5f) +- epsilon)
    result.forwardPoint.y should be ((.5f) +- epsilon)
    result.forwardPoint.z should be ((1f) +- epsilon)
  }
  
   "TrianglePlaneIntersectionTest 3" should "find intersections z-hypotenuse plane" in {  
    result.reset
    val lastPosition = new Vec3D(.5f,.5f,1f)
    val planeTriangle = new Triangle3D(new Vec3D(1,0,0), new Vec3D(0f,1f,0), new Vec3D(1f,0f,1f))
    val plane = new Plane(planeTriangle)
    val direction = new Vec3D(-1f,1f,0).normalize()
    
    TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, lastPosition, direction, result)
    //println("Result: " + result)
    
    result.hasForwardPoint should be (true)
    result.hasRetroPoint should be (true)
    
    result.retroPoint.x should be ((1f) +- epsilon)
    result.retroPoint.y should be ((0f) +- epsilon)
    result.retroPoint.z should be ((1f) +- epsilon)
    
    result.forwardPoint.x should be ((0f) +- epsilon)
    result.forwardPoint.y should be ((1f) +- epsilon)
    result.forwardPoint.z should be ((1f) +- epsilon)
  }
  
  "TrianglePlaneIntersectionTest 4" should "find intersections in xz plane" in {  
    result.reset
    val lastPosition = new Vec3D(.5f,0f,1f)
    val planeTriangle = new Triangle3D(new Vec3D(0,0,0), new Vec3D(1f,0f,0), new Vec3D(0f,0f,1f))
    val plane = new Plane(planeTriangle)
    val direction = new Vec3D(1f,0f,0).normalize()
    
    TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, lastPosition, direction, result)
    //println("Result: " + result)
    
    result.hasForwardPoint should be (true)
    result.hasRetroPoint should be (true)
    
    result.retroPoint.x should be ((0f) +- epsilon)
    result.retroPoint.y should be ((0f) +- epsilon)
    result.retroPoint.z should be ((1f) +- epsilon)
    
    result.forwardPoint.x should be ((1f) +- epsilon)
    result.forwardPoint.y should be ((0f) +- epsilon)
    result.forwardPoint.z should be ((1f) +- epsilon)
  }
  
  "TrianglePlaneIntersectionTest 5" should "find intersections in the yz plane" in {  
    result.reset
    val lastPosition = new Vec3D(0f,.5f,1f)
    val planeTriangle = new Triangle3D(new Vec3D(0,0,0), new Vec3D(0f,1f,0), new Vec3D(0f,0f,1f))
    val plane = new Plane(planeTriangle)
    val direction = new Vec3D(0f,1f,0).normalize()
    
    TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, lastPosition, direction, result)
    //println("Result: " + result)
    
    result.hasForwardPoint should be (true)
    result.hasRetroPoint should be (true)
    
    result.retroPoint.x should be ((0f) +- epsilon)
    result.retroPoint.y should be ((0f) +- epsilon)
    result.retroPoint.z should be ((1f) +- epsilon)
    
    result.forwardPoint.x should be ((0f) +- epsilon)
    result.forwardPoint.y should be ((1f) +- epsilon)
    result.forwardPoint.z should be ((1f) +- epsilon)
  }
}

