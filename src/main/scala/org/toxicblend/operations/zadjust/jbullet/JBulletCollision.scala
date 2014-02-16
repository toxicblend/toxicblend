package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.ToxicblendException
import com.bulletphysics.util.ObjectArrayList
import com.bulletphysics.collision.broadphase.BroadphaseInterface
import com.bulletphysics.collision.broadphase.AxisSweep3_32
import com.bulletphysics.collision.broadphase.DbvtBroadphase
import com.bulletphysics.collision.shapes.BvhTriangleMeshShape
import com.bulletphysics.collision.shapes.CollisionShape
import com.bulletphysics.collision.shapes.TriangleIndexVertexArray
import com.bulletphysics.collision.shapes.ConeShapeZ
import com.bulletphysics.collision.shapes.ConvexShape;
import com.bulletphysics.collision.dispatch.DefaultCollisionConfiguration
import com.bulletphysics.collision.dispatch.CollisionDispatcher
import com.bulletphysics.collision.dispatch.CollisionFlags
import com.bulletphysics.collision.dispatch.CollisionWorld
import com.bulletphysics.collision.dispatch.CollisionObject
//import com.bulletphysics.dynamics.RigidBody
//import com.bulletphysics.dynamics.RigidBodyConstructionInfo
//import com.bulletphysics.dynamics.constraintsolver.ConstraintSolver
//import com.bulletphysics.dynamics.constraintsolver.SequentialImpulseConstraintSolver
//import com.bulletphysics.dynamics.RigidBody
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.linearmath.DefaultMotionState
import org.toxicblend.typeconverters.Mesh3DConverter
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3f

class CollisionObjectWrapper(val model:Mesh3DConverter) {
  
  val collisionShapes = new ObjectArrayList[CollisionShape]();
  val convexShapes = new ArrayBuffer[ConvexShape]
  
  val vertStride = 3 * 4
  val indexStride = 3 * 4
  val totalVerts = model.getVertices.size
  val totalTriangles = model.getFaces.size
   
  val gVertices = ByteBuffer.allocateDirect(totalVerts * 3 * 4).order(ByteOrder.nativeOrder());
  val gIndices = ByteBuffer.allocateDirect(totalTriangles * 3 * 4).order(ByteOrder.nativeOrder());
  
  (0 until totalVerts).foreach(index => {
    val v = model.getVertices(index)
    gVertices.putFloat((index*3 + 0) * 4, v.x)
    gVertices.putFloat((index*3 + 1) * 4, v.y)
    gVertices.putFloat((index*3 + 2) * 4, v.z)
  });
  
  {
    //println("totalVerts:" + totalVerts)
    //println("totalTriangles:" + totalTriangles)
    val faces = model.getFaces
    (0 until totalTriangles).foreach(index => {
      val face = faces(index)
      if (face.size != 3 ) throw new ToxicblendException("JBullet mesh must be triangulated")
      else {
        gIndices.putInt((index*3 + 0) * 4, face(0))
        gIndices.putInt((index*3 + 1) * 4, face(1))
        gIndices.putInt((index*3 + 2) * 4, face(2))
      }
    })
  }
  
  val indexVertexArrays = new TriangleIndexVertexArray(totalTriangles,
        gIndices,
        indexStride,
        totalVerts, gVertices, vertStride)
  
  val useQuantizedAabbCompression = true
  
  val trimeshShape = new BvhTriangleMeshShape(indexVertexArrays, useQuantizedAabbCompression)
  collisionShapes.add(trimeshShape)

  val groundShape:CollisionShape = trimeshShape
  val collisionConfiguration = new DefaultCollisionConfiguration()
  val dispatcher = new CollisionDispatcher(collisionConfiguration)
  val broadphase:BroadphaseInterface = {
    val worldMin = JBulletUtil.convertVec3DToVector3f(model.getBounds.getMin)
    val worldMax = JBulletUtil.convertVec3DToVector3f(model.getBounds.getMax)
    //println("worldMin=" + worldMin)
    //println("worldMax=" + worldMax)
    new AxisSweep3_32(worldMin, worldMax, 1500000/2);
  }
  //val broadphase:BroadphaseInterface = new DbvtBroadphase()
  val collisionWorld = new CollisionWorld(dispatcher, broadphase, collisionConfiguration)
  val startTransform = new Transform
  startTransform.setIdentity();
  startTransform.origin.set(0f, -2f, 0f);

  val colShape:CollisionShape = new ConeShapeZ(2f,2f);
  collisionShapes.add(colShape);
  
  startTransform.setIdentity();
  var mass = 0f
  val staticBody = localCreateCollisionObject(mass, startTransform, groundShape)

  staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.STATIC_OBJECT);

  // enable custom material callback
  staticBody.setCollisionFlags(staticBody.getCollisionFlags() | CollisionFlags.CUSTOM_MATERIAL_CALLBACK);
 
  def localCreateCollisionObject(mass:Float, startTransform:Transform, shape:CollisionShape):CollisionObject = {
    // rigidbody is dynamic if and only if mass is non zero, otherwise static
    val isDynamic:Boolean = (mass != 0f)

    val localInertia = new Vector3f(0f, 0f, 0f)
    if (isDynamic) {
      shape.calculateLocalInertia(mass, localInertia)
    }
    
    val myMotionState = new DefaultMotionState(startTransform)
    val collisionobject = new CollisionObject
    collisionobject.setCollisionShape(shape)
   
    collisionWorld.addCollisionObject(collisionobject)
    collisionobject
  }
}

class JBulletCollision(val model:Mesh3DConverter) {
   val collisionWrapper = new CollisionObjectWrapper(model)
   
   def doSomething = {
     println(collisionWrapper.collisionWorld)
     println(collisionWrapper.groundShape)
   }
}