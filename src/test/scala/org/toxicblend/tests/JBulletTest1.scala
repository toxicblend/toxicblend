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


class JBulletTest1 extends FlatSpec with Matchers {
  val epsilon = 0.00001f
  val triangle = Array(new Point3dE(0,0,1), new Point3dE(1,0,1), new Point3dE(0,1,1))
  val models = {
    val toximesh = new TriangleMesh
    toximesh.addFace(new Vec3D(triangle(0).x.toFloat,triangle(0).y.toFloat,triangle(0).z.toFloat),
                     new Vec3D(triangle(1).x.toFloat,triangle(1).y.toFloat,triangle(1).z.toFloat),
                     new Vec3D(triangle(2).x.toFloat,triangle(2).y.toFloat,triangle(2).z.toFloat))
    val modelMesh3D = Mesh3DConverter(toximesh,"JBulletTest1")
    val bbModel = ByteBufferMeshConverter(modelMesh3D.toPBModel(None, None).build,false,1f)
    Array(bbModel)
  }
  
  val facade = new BulletFacade(models)
  val jbc = new Collider(facade, 0.05)
  
  def testCollider(segment:Array[Point3dE]) = {
    val result = new MutableList[IndexedSeq[Point3dE]]
    segment.sliding(2,1).foreach(s => {
      val r = jbc.collisionTestSegment(s(0),s(1), false)
      //println("from: " + s(0) + " to:" + s(1) + " = " + r.mkString("\n   ","\n   ", ""))
      result += r
    })
    result
  }
  
  "JBulletTest1-1" should "collide just fine" in {
    val segment = Array(new Point3dE(-2d,-2d,2d), new Point3dE(2d,2d,2d))      
    val result = testCollider(segment)
    //println("Result: " + result)
    result.size should be (1)
    val subResult = result(0)
    subResult.size should be (6)
    subResult(0).x should be ((segment(0).x) +- epsilon)
    subResult(0).y should be ((segment(0).y) +- epsilon)
    subResult(0).z should be ((0d) +- epsilon)
    
    subResult(1).x should be ((triangle(0).x) +- epsilon)
    subResult(1).y should be ((triangle(0).y) +- epsilon)
    subResult(1).z should be ((0d) +- epsilon)
    
    subResult(2).x should be ((triangle(0).x) +- epsilon)
    subResult(2).y should be ((triangle(0).y) +- epsilon)
    subResult(2).z should be ((triangle(0).z) +- epsilon)
    
    subResult(3).x should be ((.5d) +- epsilon)
    subResult(3).y should be ((.5d) +- epsilon)
    subResult(3).z should be ((1d) +- epsilon)
    
    subResult(4).x should be ((.5d) +- epsilon)
    subResult(4).y should be ((.5d) +- epsilon)
    subResult(4).z should be ((0d) +- epsilon)
    
    subResult.last.x should be ((segment.last.x) +- epsilon)
    subResult.last.y should be ((segment.last.y) +- epsilon)
    //
  }
  
  "JBulletTest1-2" should "collide along the edge just fine" in {
           
    val segment = Array(new Point3dE(0d,-1d,2d), new Point3dE(0d,2d,2d))      
    val result = testCollider(segment)
    
    //println("Result: " + result)
    result.size should be (1)
    val subResult = result(0)
    subResult.size should be (6)
    subResult(0).x should be ((segment(0).x) +- epsilon)
    subResult(0).y should be ((segment(0).y) +- epsilon)
    
    subResult(1).x should be ((triangle(0).x) +- epsilon)
    subResult(1).y should be ((triangle(0).y) +- epsilon)
    subResult(1).z should be ((0d) +- epsilon)
    
    subResult(2).x should be ((triangle(0).x) +- epsilon)
    subResult(2).y should be ((triangle(0).y) +- epsilon)
    subResult(2).z should be ((triangle(0).z) +- epsilon)
    
    subResult(3).x should be ((triangle(2).x) +- epsilon)
    subResult(3).y should be ((triangle(2).y) +- epsilon)
    subResult(3).z should be ((triangle(2).z) +- epsilon)
    
    subResult(4).x should be ((triangle(2).x) +- epsilon)
    subResult(4).y should be ((triangle(2).y) +- epsilon)
    subResult(4).z should be ((0d) +- epsilon)
    
    subResult.last.x should be ((segment.last.x)+- epsilon)
    subResult.last.y should be ((segment.last.y)+- epsilon)

  }
  
  "jbullet cleanup" should "not fail" in {
    facade.destroy
  }
  
}