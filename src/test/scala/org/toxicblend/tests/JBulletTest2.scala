package org.toxicblend.tests

import toxi.geom.mesh.TriangleMesh
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.ByteBufferMeshConverter
import scala.collection.mutable.MutableList
import org.toxicblend.operations.zadjust.BulletFacade
import org.toxicblend.operations.zadjust.Collider
import com.bulletphysics.linearmath.Point3dE
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
    val modelMesh3D = Mesh3DConverter(toximesh,"JBulletTest2")
    val bbModel = ByteBufferMeshConverter(modelMesh3D.toPBModel(None, None).build,false,1f)
    Array(bbModel)
  }
  
  
  def doTheRayTests(segment:Array[Point3dE]) = {
    val result = new MutableList[IndexedSeq[Point3dE]]
    segment.sliding(2,1).foreach(s => {
      val r = jbc.collisionTestSegment(s(0),s(1), false)
      //println("from: " + s(0) + " to:" + s(1) + " = " + r.mkString("\n   ","\n   ", ""))
      result += r
    })
    result
  }
  
  val facade = new BulletFacade(models)
  val jbc = new Collider(facade, 0.005f)
  
  "JBulletTest2-1" should "collide just fine" in {    
    val segment = Array(new Point3dE(0f,-1f,1f), 
                        new Point3dE(-1f,0f,1f),
                        new Point3dE(0f,1f,1f),
                        new Point3dE(1f,0f,1f),
                        new Point3dE(0f,-1f,1f))                                       
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "JBulletTest2-2" should "collide just fine" in {       
    val segment = Array(new Point3dE(1f,0f,1f),
                        new Point3dE(0f,-1f,1f), 
                        new Point3dE(-1f,0f,1f),
                        new Point3dE(0f,1f,1f),
                        new Point3dE(1f,0f,1f))      
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "JBulletTest2-3" should "collide just fine" in {  
    val segment = Array(new Point3dE(-1f,0f,1f),
                        new Point3dE(0f,1f,1f),
                        new Point3dE(1f,0f,1f),
                        new Point3dE(0f,-1f,1f),
                        new Point3dE(-1f,0f,1f))                                 
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "JBulletTest2-4" should "collide just fine" in {  
    val segment = Array(new Point3dE(-1f,0f,1f),
                        new Point3dE(0f,-1f,1f),
                        new Point3dE(1f,0f,1f), 
                        new Point3dE(0f,1f,1f),
                        new Point3dE(-1f,0f,1f))                                 
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment.mkString("\n") )
    //println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "JBulletTest2-edge-1" should "collide just fine" in {  
    val segment = Array(new Point3dE(-1f,-1f,1f),
                        new Point3dE(0f,0f,1f),
                        new Point3dE(1f,1f,1f))                                 
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment.mkString("\n") )
    //println("Result segments:\n" + result.mkString("\n"))
    val firstI = segment(0)
    val lastI = segment.last
    val firstO = result(0)(0)
    val lastO = result.last.last
    //println("firstI: " +firstI + " firstO=" +firstO)
    //println("lastI: " +lastI + " lastO=" +lastO)
    
    result.size should be (2)
     
    firstO.x should be (firstI.x)
    firstO.y should be (firstI.y)
    firstO.z should be ((0d) plusOrMinus 1e-5f) 
    
    lastO.x should be (lastI.x)
    lastO.y should be (lastI.y)
    lastO.z should be ((0d) plusOrMinus 1e-5f)
    
  }
  
  "JBulletTest2-edge-2" should "collide just fine" in {  
    val segment = Array(new Point3dE(1f,-1f,1f),
                        new Point3dE(0f,0f,1f),
                        new Point3dE(-1f,1f,1f))                                 
    val result = doTheRayTests(segment)
    //println("input Segment: " + segment.mkString("\n") )
    //println("Result segments:\n" + result.mkString("\n"))
    val firstI = segment(0)
    val lastI = segment.last
    val firstO = result(0)(0)
    val lastO = result.last.last
    //println("firstI: " +firstI + " firstO=" +firstO)
    //println("lastI: " +lastI + " lastO=" +lastO)
    
    result.size should be (2)
     
    firstO.x should be (firstI.x)
    firstO.y should be (firstI.y)
    firstO.z should be ((0d) plusOrMinus 1e-5f) 
    
    lastO.x should be (lastI.x)
    lastO.y should be (lastI.y)
    lastO.z should be ((0d) plusOrMinus 1e-5f)
    
  }
  
  "JBulletTest2 cleanup" should "not fail" in {
    facade.destroy
  }
}