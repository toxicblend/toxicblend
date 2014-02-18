package org.toxicblend.operations.zadjust.jbullet;

import toxi.geom.mesh.TriangleMesh
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import com.bulletphysics.util.ObjectArrayList
import com.bulletphysics.collision.broadphase.BroadphaseInterface
import com.bulletphysics.collision.broadphase.AxisSweep3_32
import com.bulletphysics.collision.broadphase.DbvtBroadphase
import com.bulletphysics.collision.shapes.BvhTriangleMeshShape
import com.bulletphysics.collision.shapes.CollisionShape
import com.bulletphysics.collision.shapes.TriangleIndexVertexArray
import com.bulletphysics.collision.shapes.ConeShapeZ
import com.bulletphysics.collision.shapes.ConvexShape
import com.bulletphysics.collision.shapes.UniformScalingShape
import com.bulletphysics.collision.dispatch.DefaultCollisionConfiguration
import com.bulletphysics.collision.dispatch.CollisionDispatcher
import com.bulletphysics.collision.dispatch.CollisionFlags
import com.bulletphysics.collision.dispatch.CollisionWorld
import com.bulletphysics.collision.dispatch.CollisionObject
import com.bulletphysics.collision.dispatch.CollisionWorld.RayResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.ClosestRayResultCallback
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.linearmath.DefaultMotionState;
import org.toxicblend.typeconverters.Mesh3DConverter;
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3f
import com.bulletphysics.linearmath.VectorUtil

class MyRayCallback(val rayFromWorld:Vector3f, val rayToWorld:Vector3f) extends RayResultCallback {
  val hitPointWorld = new Vector3f
  override def addSingleResult(rayResult:LocalRayResult, normalInWorldSpace:Boolean):Float = {
    closestHitFraction = rayResult.hitFraction      
    VectorUtil.setInterpolate3(hitPointWorld, rayFromWorld, rayToWorld, rayResult.hitFraction)
    rayResult.hitFraction
  }
}

object JBulletTest {
 
  def main(args: Array[String]): Unit = {
    val toximesh = new TriangleMesh
    toximesh.addFace(new Vec3D(0,0,0), new Vec3D(10,0,0), new Vec3D(0,10,0))
    val model = Mesh3DConverter(toximesh,"test")
    val models = Array(model)
    val segment:Array[ReadonlyVec3D] = Array(new Vec3D(-10,-10,1), new Vec3D(10,10,1))
    val collisionWrapper = CollisionObjectWrapper(segment, models)
    val dud = new Vector3f(0,0,0)
    val boundsMin = model.getBounds.getMin
    val boundsMax = model.getBounds.getMax
    println("BB min:" + boundsMin + " max: " + boundsMax )
    val deltaX =  (boundsMax.x - boundsMin.x) / 50f
    val deltaY =  (boundsMax.y - boundsMin.y) / 50f
    
    val rayFromWorld = new Vector3f(boundsMin.x,boundsMin.y+3,boundsMax.z+1)
    val rayToWorld = new Vector3f(rayFromWorld.x,rayFromWorld.y,boundsMin.z-1)
   
    val resultCallback = new MyRayCallback(rayFromWorld,rayToWorld)  
    (0 until 50).foreach(i => {
      rayFromWorld.x += deltaX
      rayToWorld.x += deltaX
      rayFromWorld.y += deltaY
      rayToWorld.y += deltaY
     
      println("testing " + resultCallback.rayFromWorld + "->" + resultCallback.rayToWorld )
      collisionWrapper.collisionWorld.rayTest(resultCallback.rayFromWorld, resultCallback.rayToWorld, resultCallback)
      if (resultCallback.closestHitFraction != 1f) {  
        println("" + resultCallback.rayFromWorld + "->" + resultCallback.rayToWorld + " ==> " + resultCallback.hitPointWorld )
        resultCallback.closestHitFraction = 1f
      }
      println
    })
    collisionWrapper.destroy
  }
}