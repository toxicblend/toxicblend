package org.toxicblend.tests

import toxi.geom.mesh.TriangleMesh
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import org.toxicblend.typeconverters.Mesh3DConverter
import scala.collection.mutable.MutableList
import org.toxicblend.operations.zadjust.jbullet.JBulletCollision
import org.scalatest._
import matchers.ShouldMatchers._

/**
 * this is a failed test i noticed while playing around in blender
 */
class JBulletTest2 extends FlatSpec with Matchers {
  
  val models = {
    val toximesh = new TriangleMesh
    toximesh.addFace(new Vec3D( 2,-2,0), new Vec3D(-2,-2,0), new Vec3D(-2, 2,0))
    toximesh.addFace(new Vec3D(-2, 2,0), new Vec3D( 2, 2,0), new Vec3D( 2,-2,0))
    val model = Mesh3DConverter(toximesh,"JBulletTest2")
    Array(model)
  }
  
  def doTheRayTests(segment:Array[ReadonlyVec3D]) = {
    val result = new MutableList[IndexedSeq[ReadonlyVec3D]]
    segment.sliding(2,1).foreach(s => {
      val r = jbc.doCollisionTests(s)
      //println("from: " + s(0) + " to:" + s(1) + " = " + r.mkString("\n   ","\n   ", ""))
      result += r.flatten
    })
    result
  }
  
  val jbc = new JBulletCollision(models, 0.005f, 0.0001f)
  "jbullet2-1" should "collide just fine" in {    
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(0f,-1f,1f), 
                                             new Vec3D(-1f,0f,1f),
                                             new Vec3D(0f,1f,1f),
                                             new Vec3D(1f,0f,1f),
                                             new Vec3D(0f,-1f,1f))                                       
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "jbullet2-2" should "collide just fine" in {       
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(1f,0f,1f),
                                             new Vec3D(0f,-1f,1f), 
                                             new Vec3D(-1f,0f,1f),
                                             new Vec3D(0f,1f,1f),
                                             new Vec3D(1f,0f,1f))      
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "jbullet2-3" should "collide just fine" in {  
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(-1f,0f,1f),
                                             new Vec3D(0f,1f,1f),
                                             new Vec3D(1f,0f,1f),
                                             new Vec3D(0f,-1f,1f),
                                             new Vec3D(-1f,0f,1f))                                 
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "jbullet2-4" should "collide just fine" in {  
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(-1f,0f,1f),
                                             new Vec3D(0f,-1f,1f),
                                             new Vec3D(1f,0f,1f), 
                                             new Vec3D(0f,1f,1f),
                                             new Vec3D(-1f,0f,1f))                                 
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment.mkString("\n") )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "jbullet-edge-1" should "collide just fine" in {  
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(-1f,-1f,1f),
                                             new Vec3D(0f,0f,1f),
                                             new Vec3D(1f,1f,1f))                                 
    val result = doTheRayTests(segment)
    println("input Segment: " + segment.mkString("\n") )
    println("Result segments:\n" + result.mkString("\n"))
    val firstI = segment(0)
    val lastI = segment.last
    val firstO = result(0)(0)
    val lastO = result.last.last
    println("firstI: " +firstI + " firstO=" +firstO)
    println("lastI: " +lastI + " lastO=" +lastO)
    
    result.size should be (2)
     
    firstO.x should be (firstI.x)
    firstO.y should be (firstI.y)
    firstO.z should be ((0f) plusOrMinus 1e-5f) 
    
    lastO.x should be (lastI.x)
    lastO.y should be (lastI.y)
    lastO.z should be ((0f) plusOrMinus 1e-5f)
    
  }
  
  "jbullet cleanup" should "not fail" in {
    jbc.cleanup
  }
}