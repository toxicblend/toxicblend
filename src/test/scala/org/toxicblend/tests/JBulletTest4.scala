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
class JBulletTest4 extends FlatSpec with Matchers {
  
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
  
  
  "jbulletTest3-1" should "collide just fine" in {    
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
  
  "jbullet cleanup" should "not fail" in {
    jbc.cleanup
  }
}