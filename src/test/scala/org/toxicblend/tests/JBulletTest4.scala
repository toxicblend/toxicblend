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
class JBulletTest4 extends FlatSpec with Matchers {
  
  val models = {
    val toximesh = new TriangleMesh
    toximesh.addFace(new Vec3D( 2,-2,0), new Vec3D(-2,-2,0), new Vec3D(-2, 2,0))
    toximesh.addFace(new Vec3D(-2, 2,0), new Vec3D( 2, 2,0), new Vec3D( 2,-2,0))
    val modelMesh3D = Mesh3DConverter(toximesh,"JBulletTest4")
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
  val jbc = new Collider(facade, 0.005f, 0.0001f) 
  
  "JBulletTest4-1" should "collide just fine" in {    
    val segment = Array(new Point3dE(0f,-1f,1f), 
                        new Point3dE(-1f,0f,1f),
                        new Point3dE(0f,1f,1f),
                        new Point3dE(1f,0f,1f),
                        new Point3dE(0f,-1f,1f))                                       
    val result = doTheRayTests(segment)
    println("input Segment: " + segment.mkString("\n") )
    println("Result segments:\n" + result.mkString("\n"))
    val first = result(0)(0)
    val last = result.last.last
    last.x should be (first.x)
    last.y should be (first.y)
    last.z should be (first.z)
  }
  
  "jbullet cleanup" should "not fail" in {
    facade.destroy
  }
}