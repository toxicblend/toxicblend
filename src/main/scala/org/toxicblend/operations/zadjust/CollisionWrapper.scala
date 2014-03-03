package org.toxicblend.operations.zadjust

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
import org.toxicblend.typeconverters.ByteBufferMeshConverter
import scala.collection.mutable.ArrayBuffer
import javax.vecmath.AxisAngle4d
import javax.vecmath.Matrix3d
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3d
import javax.vecmath.Point3d
import javax.vecmath.Point2d
import toxi.geom.Vec3D
import com.bulletphysics.linearmath.AABB

/**
 * Container for all things jbullet: collision objects, collisionWorld etc, etc
 */
class CollisionWrapper(val models:IndexedSeq[ByteBufferMeshConverter]) {
  
  val collisionShapes = new ObjectArrayList[CollisionShape]();
  val convexShapes = new ArrayBuffer[ConvexShape]
  assert(models.size > 0)
  val aabbAllModels = new AABB(models.map(m=>m.aabb))
  
  val collisionWorld = {
    val collisionConfiguration = new DefaultCollisionConfiguration
    val dispatcher = new CollisionDispatcher(collisionConfiguration)
    val broadphase:BroadphaseInterface = if (false) {
        val aabb = new AABB(aabbAllModels); aabb.getMax.z += 1; ; aabb.getMin.z -= 1
        new AxisSweep3_32(aabb.getMin, aabb.getMax, 1500000/2);
      } else {
      new DbvtBroadphase
    }
    new CollisionWorld(dispatcher, broadphase, collisionConfiguration)
  }
  
  
  models.foreach(model => {
    // public TriangleIndexVertexArray(int numTriangles, ByteBuffer triangleIndexBase, int triangleIndexStride, 
    //                                 int numVertices, ByteBuffer vertexBase, int vertexStride) {
    val indexVertexArray = new TriangleIndexVertexArray(model.totalTriangles, 
                                                        model.gIndices, 
                                                        ByteBufferMeshConverter.T_INDEX_STRIDE, 
                                                        model.totalVerts, 
                                                        model.gVertices, 
                                                        ByteBufferMeshConverter.VERTEX_STRIDE)
    
    val useQuantizedAabbCompression = true
    val trimeshShape = new BvhTriangleMeshShape(indexVertexArray, useQuantizedAabbCompression)
    //trimeshShape.setMargin(0.1)
    collisionShapes.add(trimeshShape)
    val startTransform = new Transform
    startTransform.setIdentity();
    startTransform.origin.set(0f, 0f, 0f);
    val staticBody = localCreateCollisionObject(startTransform, trimeshShape)
    staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.STATIC_OBJECT);
  })
  
  class ConeShapeZContainer (val radius:Double, val height:Double, margin:Double) {
    val zAdjust = -height*0.5
    // ConeShapeZ should be pointing down in Z
    val rotation = {
      val m = new Matrix3d; m.set(new AxisAngle4d(new Vector3d(1,0,0), math.Pi)); m
    }
    val shape = addConeShapeZ(radius-margin, height-margin, margin)  
  }
  
  val coneShapeZ = new ConeShapeZContainer(.0001, .0001, 0.00001)
  
  val convexCallback = new ClosestConvexResultCallback(coneShapeZ.rotation,coneShapeZ.zAdjust,aabbAllModels.getMin.z-1, aabbAllModels.getMax.z+1)
  val rayCallback = new ClosestRayResultCallback(aabbAllModels.getMin.z-1, aabbAllModels.getMax.z+1)
  
  // enable custom material callback
  //staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.CUSTOM_MATERIAL_CALLBACK);
  
  def localCreateCollisionObject(startTransform:Transform, shape:CollisionShape):CollisionObject = {   
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
  
  def collisionTestPoint(point:Point2d):Vec3D = {
    rayCallback.resetForReuse(point)
    collisionWorld.rayTest(rayCallback.rayFromWorld, rayCallback.rayToWorld, rayCallback)
    val result = if (rayCallback.hasResult){
      rayCallback.getResult
    } else {
      convexCallback.resetForReuse(point)
      collisionWorld.convexSweepTest(coneShapeZ.shape, convexCallback.fromT, convexCallback.toT, convexCallback)
      println("convexCallback hit " + convexCallback.hasResult)
      convexCallback.getResult
    }
    new Vec3D(result.x.toFloat, result.y.toFloat, result.z.toFloat)
  }
  
  def collisionTestSegment(point1:Point2d, point2:Point2d):IndexedSeq[Vec3D] = {
    val rv = new ArrayBuffer[Vec3D]
    rv += collisionTestPoint(point1)
    rv += collisionTestPoint(point2)
    rv
  }
}