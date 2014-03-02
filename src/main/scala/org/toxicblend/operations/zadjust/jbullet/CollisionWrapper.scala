package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.ToxicblendException
import org.toxicblend.geometry.TrianglePlaneIntersection
import org.toxicblend.geometry.TrianglePlaneIntersectionResult
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
import com.bulletphysics.collision.dispatch.CollisionWorld.LocalRayResult
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.linearmath.DefaultMotionState
import com.bulletphysics.linearmath.VectorUtil
import org.toxicblend.typeconverters.Mesh3DConverter
import scala.collection.mutable.ArrayBuffer
import javax.vecmath.AxisAngle4d
import javax.vecmath.Matrix3d
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3d
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Plane
import toxi.geom.Ray3D
import toxi.geom.AABB

class CollisionWrapper(val models:IndexedSeq[Mesh3DConverter]) {
  
  val collisionShapes = new ObjectArrayList[CollisionShape]();
  val convexShapes = new ArrayBuffer[ConvexShape]
  
  val vertStride = 3 * CollisionWrapper.VERTEX_S
  val indexStride = 3 * CollisionWrapper.INDEX_S
  val totalVerts = models(0).getVertices.size
  val totalTriangles = models(0).getFaces.size
   
  val gVertices = ByteBuffer.allocateDirect(totalVerts*vertStride).order(ByteOrder.nativeOrder());
  val gIndices = ByteBuffer.allocateDirect(totalTriangles*indexStride).order(ByteOrder.nativeOrder());
  
  val aabbAllModels = {
    if (models.size > 0) {
      val aabbTmp = models(0).getBounds.copy
      models.tail.foreach(b => aabbTmp.union(b.getBounds))
      aabbTmp
    } else {
      new AABB
    }
  }
  val zMin = aabbAllModels.getMin.z-1d
  val zMax = aabbAllModels.getMax.z+1d
  
  (0 until totalVerts).foreach(index => {
    val v = models(0).getVertices(index)
    gVertices.putDouble((index*3 + 0) * CollisionWrapper.VERTEX_S, v.x)
    gVertices.putDouble((index*3 + 1) * CollisionWrapper.VERTEX_S, v.y)
    gVertices.putDouble((index*3 + 2) * CollisionWrapper.VERTEX_S, v.z)
  });
  
  {
    //println("totalVerts:" + totalVerts)
    //println("totalTriangles:" + totalTriangles)
    val faces = models(0).getFaces
    (0 until totalTriangles).foreach(index => {
      val face = faces(index)
      if (face.size > 3 ) throw new ToxicblendException("JBullet mesh must be triangulated")
      else if (face.size == 3){
        gIndices.putInt((index*3 + 0) * CollisionWrapper.INDEX_S, face(0))
        gIndices.putInt((index*3 + 1) * CollisionWrapper.INDEX_S, face(1))
        gIndices.putInt((index*3 + 2) * CollisionWrapper.INDEX_S, face(2))
      }
      // silently ignore edges and unconnected vertices
    })
  }
  
  val indexVertexArrays = new TriangleIndexVertexArray(totalTriangles, gIndices, indexStride, totalVerts, gVertices, vertStride)
  val useQuantizedAabbCompression = true
  val trimeshShape = new BvhTriangleMeshShape(indexVertexArrays, useQuantizedAabbCompression)
  collisionShapes.add(trimeshShape)

  val groundShape:CollisionShape = trimeshShape
  val collisionConfiguration = new DefaultCollisionConfiguration()
  val dispatcher = new CollisionDispatcher(collisionConfiguration)
  val broadphase:BroadphaseInterface = if (true) {
      val worldMin = JBulletUtil.vec3DToNewVector3d(aabbAllModels.getMin)
      val worldMax = JBulletUtil.vec3DToNewVector3d(aabbAllModels.getMax)
      new AxisSweep3_32(worldMin, worldMax, 1500000/2);
    } else {
    new DbvtBroadphase
  }
  
  //val broadphase:BroadphaseInterface = new DbvtBroadphase()
  val collisionWorld = new CollisionWorld(dispatcher, broadphase, collisionConfiguration)
  val startTransform = new Transform
  startTransform.setIdentity();
  startTransform.origin.set(0f, 0f, 0f);
  val staticBody = localCreateCollisionObject(startTransform, groundShape)
  
  class ConeShapeZContainer (val radius:Double, val height:Double, margin:Double) {
    val zAdjust = height/2d
    // ConeShapeZ should be pointing down in Z
    val rotation = {
      val m = new Matrix3d; m.set(new AxisAngle4d(new Vector3d(1,0,0), math.Pi)); m
    }
    val shape = addConeShapeZ(radius-margin, height-margin, margin)  
  }
  
  val coneShapeZ = new ConeShapeZContainer(0.01, 0.05, 0.001)
  
  staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.STATIC_OBJECT);

  // enable custom material callback
  //staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.CUSTOM_MATERIAL_CALLBACK);
  
  def localCreateCollisionObject(startTransform:Transform, shape:CollisionShape):CollisionObject = {   
    val myMotionState = new DefaultMotionState(startTransform)
    val collisionobject = new CollisionObject
    collisionobject.setCollisionShape(shape)
    collisionobject.setWorldTransform(startTransform)
    collisionWorld.addCollisionObject(collisionobject)
    collisionobject
  }
  
  def addConeShapeZ(radius:Double, height:Double, margin:Double):ConvexShape = {
    val colShape:ConvexShape = new ConeShapeZ(radius, height);
    colShape.setMargin(margin)
    collisionShapes.add(colShape);
    val convexShape = new UniformScalingShape(colShape, 1f)
    convexShapes.append(convexShape)
    convexShape
  }
  
  def destroy = collisionWorld.destroy
}

object CollisionWrapper {
  val VERTEX_S = 8 // a double
  val INDEX_S = 4  // an int
}