package org.toxicblend.tests

import toxi.geom.mesh.TriangleMesh
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import org.toxicblend.typeconverters.Mesh3DConverter
import scala.collection.mutable.MutableList
import org.toxicblend.operations.zadjust.jbullet.JBulletCollision
import org.scalatest._


class JBulletTest1 extends FlatSpec with Matchers {
  val epsilon = 0.00001f
  val triangle = Array(new Vec3D(0,0,1), new Vec3D(1,0,1), new Vec3D(0,1,1))
  val models = {
    val toximesh = new TriangleMesh
    toximesh.addFace(triangle(0), triangle(1), triangle(2))
    val model = Mesh3DConverter(toximesh,"JBulletTest1")
    Array(model)
  }
  
  val jbc = new JBulletCollision(models, 0.005f, 0.0001f)
  
  def testCollider(segment:Array[ReadonlyVec3D]) = {
    val result = new MutableList[IndexedSeq[ReadonlyVec3D]]
    segment.sliding(2,1).foreach(s => {
      val r = jbc.doCollisionTests(s)
      //println("from: " + s(0) + " to:" + s(1) + " = " + r.mkString("\n   ","\n   ", ""))
      result += r.flatten
    })
    result
  }
  
  "jbullet1" should "collide just fine" in {
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(-2f,-2f,2f), new Vec3D(2f,2f,2f))      
    val result = testCollider(segment)
    println("Result: " + result)
    result.size should be (1)
    val subResult = result(0)
    subResult.size should be (6)
    subResult(0).x should be ((segment(0).x) +- epsilon)
    subResult(0).y should be ((segment(0).y) +- epsilon)
    subResult(0).z should be ((0f) +- epsilon)
    
    subResult(1).x should be ((triangle(0).x) +- epsilon)
    subResult(1).y should be ((triangle(0).y) +- epsilon)
    subResult(1).z should be ((0f) +- epsilon)
    
    subResult(2).x should be ((triangle(0).x) +- epsilon)
    subResult(2).y should be ((triangle(0).y) +- epsilon)
    subResult(2).z should be ((triangle(0).z) +- epsilon)
    
    subResult(3).x should be ((.5f) +- epsilon)
    subResult(3).y should be ((.5f) +- epsilon)
    subResult(3).z should be ((1f) +- epsilon)
    
    subResult(4).x should be ((.5f) +- epsilon)
    subResult(4).y should be ((.5f) +- epsilon)
    subResult(4).z should be ((0f) +- epsilon)
    
    subResult.last.x should be ((segment.last.x) +- epsilon)
    subResult.last.y should be ((segment.last.y) +- epsilon)
    //
  }
  
  "jbullet2" should "collide along the edge just fine" in {
           
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(0f,-1f,2f), new Vec3D(0f,2f,2f))      
    val result = testCollider(segment)
    
    //println("Result: " + result)
    result.size should be (1)
    val subResult = result(0)
    subResult.size should be (6)
    subResult(0).x should be ((segment(0).x) +- epsilon)
    subResult(0).y should be ((segment(0).y) +- epsilon)
    
    subResult(1).x should be ((triangle(0).x) +- epsilon)
    subResult(1).y should be ((triangle(0).y) +- epsilon)
    subResult(1).z should be ((0f) +- epsilon)
    
    subResult(2).x should be ((triangle(0).x) +- epsilon)
    subResult(2).y should be ((triangle(0).y) +- epsilon)
    subResult(2).z should be ((triangle(0).z) +- epsilon)
    
    subResult(3).x should be ((triangle(2).x) +- epsilon)
    subResult(3).y should be ((triangle(2).y) +- epsilon)
    subResult(3).z should be ((triangle(2).z) +- epsilon)
    
    subResult(4).x should be ((triangle(2).x) +- epsilon)
    subResult(4).y should be ((triangle(2).y) +- epsilon)
    subResult(4).z should be ((0f) +- epsilon)
    
    subResult.last.x should be ((segment.last.x)+- epsilon)
    subResult.last.y should be ((segment.last.y)+- epsilon)

  }
  
  "jbullet cleanup" should "not fail" in {
    jbc.cleanup
  }
  
}