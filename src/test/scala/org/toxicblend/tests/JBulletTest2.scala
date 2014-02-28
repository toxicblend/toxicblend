package org.toxicblend.tests

import toxi.geom.mesh.TriangleMesh
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import org.toxicblend.typeconverters.Mesh3DConverter
import scala.collection.mutable.MutableList
import org.toxicblend.operations.zadjust.jbullet.JBulletCollision
import org.scalatest._

/**
 * this is a failed test i noticed while playing around in blender
 */
class JBulletTest2 extends FlatSpec with Matchers {
  
  val models = {
    val toximesh = new TriangleMesh
    toximesh.addFace(new Vec3D(1,-1,0), new Vec3D(-1,-1,0), new Vec3D(-1,1,0))
    toximesh.addFace(new Vec3D(-1,1,0), new Vec3D(1,1,0), new Vec3D(1,-1,0))
    val model = Mesh3DConverter(toximesh,"JBulletTest2")
    Array(model)
  }
  
  val segments:Array[IndexedSeq[ReadonlyVec3D]] = {
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(0f,-.7071068f,1f), 
                                             new Vec3D(-.7071068f,0f,1f),
                                             new Vec3D(.7071068f,0f,1f),
                                             new Vec3D(.7071068f,0f,1f))
    Array(segment)
  }
  val jbc = new JBulletCollision(segments, models, 0.005f, 0.0001f)
  
  "jbullet2" should "collide just fine" in {
           
    val result = new MutableList[IndexedSeq[ReadonlyVec3D]]
    segments.foreach(segment => {
      segment.sliding(2,1).foreach(s => {
        val r = jbc.doRayTests(s)
        //println("from: " + s(0) + " to:" + s(1) + " = " + r.mkString("\n   ","\n   ", ""))
        result += r.flatten
      })
    })
    println("Segment: " + segments(0))
    println("Result: " + result.mkString("\n"))
    //result.size should be (4)
    val first = result(0)(0)
    val last = result.last.last
    //val firstInput = segments(0)(0)
    //val LastInput = segments(0).last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
    
    /*val subResult = result(0)
    subResult.size should be (4)
    subResult(0).x should be (segments(0)(0).x)
    subResult(0).y should be (segments(0)(0).y)
    subResult.last.x should be (segments(0).last.x)
    subResult.last.y should be (segments(0).last.y)
    *///
  }
  
  "jbullet cleanup" should "not fail" in {
    jbc.cleanup
  }
  
}