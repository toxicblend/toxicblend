package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.ToxicblendException
import org.toxicblend.geometry.TrianglePlaneIntersection
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
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3f
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Plane
import toxi.geom.Ray3D
import toxi.geom.AABB

class SpecialRayCallback(var rayFromWorld:Vector3f, var rayToWorld:Vector3f) extends RayResultCallback {
  val hitPointWorld = new Vector3f
  var triangleIndex:Int = -1
  
  override def addSingleResult(rayResult:LocalRayResult, normalInWorldSpace:Boolean):Float = {
    closestHitFraction = rayResult.hitFraction      
    VectorUtil.setInterpolate3(hitPointWorld, rayFromWorld, rayToWorld, rayResult.hitFraction)
    triangleIndex = rayResult.localShapeInfo.triangleIndex
    rayResult.hitFraction
  }
}

class CollisionObjectWrapper(val segments:IndexedSeq[IndexedSeq[ReadonlyVec3D]], val models:IndexedSeq[Mesh3DConverter]) {
  
  val collisionShapes = new ObjectArrayList[CollisionShape]();
  val convexShapes = new ArrayBuffer[ConvexShape]
  
  val vertStride = 3 * 4
  val indexStride = 3 * 4
  val totalVerts = models(0).getVertices.size
  val totalTriangles = models(0).getFaces.size
   
  val gVertices = ByteBuffer.allocateDirect(totalVerts * 3 * 4).order(ByteOrder.nativeOrder());
  val gIndices = ByteBuffer.allocateDirect(totalTriangles * 3 * 4).order(ByteOrder.nativeOrder());
  
  (0 until totalVerts).foreach(index => {
    val v = models(0).getVertices(index)
    gVertices.putFloat((index*3 + 0) * 4, v.x)
    gVertices.putFloat((index*3 + 1) * 4, v.y)
    gVertices.putFloat((index*3 + 2) * 4, v.z)
  });
  
  {
    //println("totalVerts:" + totalVerts)
    //println("totalTriangles:" + totalTriangles)
    val faces = models(0).getFaces
    (0 until totalTriangles).foreach(index => {
      val face = faces(index)
      if (face.size > 3 ) throw new ToxicblendException("JBullet mesh must be triangulated")
      else if (face.size == 3){
        gIndices.putInt((index*3 + 0) * 4, face(0))
        gIndices.putInt((index*3 + 1) * 4, face(1))
        gIndices.putInt((index*3 + 2) * 4, face(2))
      }
      // silently ignore edges
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
  val broadphase:BroadphaseInterface = if (true) {
    val worldMin = JBulletUtil.convertVec3DToVector3f(models(0).getBounds.getMin)
    val worldMax = JBulletUtil.convertVec3DToVector3f(models(0).getBounds.getMax)
    //println("worldMin=" + worldMin)
    //println("worldMax=" + worldMax)
    new AxisSweep3_32(worldMin, worldMax, 1500000/2);
  } else {
    new DbvtBroadphase
  }
  
  //val broadphase:BroadphaseInterface = new DbvtBroadphase()
  val collisionWorld = new CollisionWorld(dispatcher, broadphase, collisionConfiguration)
  val startTransform = new Transform
  startTransform.setIdentity();
  startTransform.origin.set(0f, 0f, 0f);
  startTransform.setIdentity();
  val staticBody = localCreateCollisionObject(startTransform, groundShape)

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
  
  def addVCutter(radius:Float, height:Float):ConvexShape = {
    val margin = 0.02f;
     val colShape:ConvexShape = new ConeShapeZ(2f,2f);
     colShape.setMargin(margin)
     collisionShapes.add(colShape);
     val convexShape = new UniformScalingShape(colShape,1f)
     convexShapes.append(convexShape)
     convexShape
  }
  
  def destroy = {
    collisionWorld.destroy
  }
}


object CollisionObjectWrapper {
  /**
   * Alternative constructor
   */
  def apply(__segment:IndexedSeq[ReadonlyVec3D], __models:IndexedSeq[Mesh3DConverter]) = {
    new CollisionObjectWrapper(Array(__segment),__models)
  }
}

class JBulletCollision(val segments:IndexedSeq[IndexedSeq[ReadonlyVec3D]], val models:IndexedSeq[Mesh3DConverter]) {
   val collisionWrapper = new CollisionObjectWrapper(segments,models)
   val aabbAllModels = {
     if (models.size > 0){
       var aabb = models(0).getBounds.copy
       (1 until models.size).foreach(b => aabb.union(models(b).getBounds))
       aabb
     }else{
       new AABB
     }
   }
   val zMin = aabbAllModels.getMin.z-1f
   val zMax = aabbAllModels.getMax.z+1f
   
   /**
    * returns the squared distance between two vertices when the Z coordinate is ignored
    */
   @inline
   def distanceToSquaredXYPlane(v0:ReadonlyVec3D, v1:ReadonlyVec3D):Float = {
     val dx = v0.x - v1.x
     val dy = v0.y - v1.y
     dx*dx + dy*dy;       
   } 
   
   def doRayTests(segments:IndexedSeq[ReadonlyVec3D]):IndexedSeq[ReadonlyVec3D] = {
     //println(collisionWrapper.collisionWorld)
     //println(collisionWrapper.groundShape)
     //val cs = collisionWrapper.addVCutter(2f,1f)
     //println(cs)
     val aabb =  models(0).getBounds.copy()
     val deltaStep = 0.005f;
     models.foreach(model => aabb.union(model.getBounds))
     
     println("BB min:" + aabbAllModels.getMin + " max: " + aabbAllModels.getMax + " zMin=" + zMin + " zMax=" + zMax)
     val rayResult = new ArrayBuffer[ReadonlyVec3D]
     val resultCallback = new SpecialRayCallback(new Vector3f(1, 1, models(0).getBounds.getMax.z), new Vector3f(1, 1, models(0).getBounds.getMin.z))
         
     segments.sliding(2,1).foreach(segment => {
       val fromV = segment(0)
       val toV = segment(1)
       val distanceToCoverSquared = distanceToSquaredXYPlane(fromV,toV)
       val plane = TrianglePlaneIntersection.segmentToZPlane(fromV,toV)
       //val steps = (.5f + fromV.distanceTo(toV)/deltaStep).intValue
       //println("Steps = " + steps)
       val direction = {
         val d = toV.sub(fromV)
         d.z = 0
         d.normalize().scaleSelf(deltaStep)
       }
       
       resultCallback.rayFromWorld = JBulletUtil.convertVec3DToVector3f(fromV)
       resultCallback.rayFromWorld.z = zMax
       resultCallback.rayToWorld = new Vector3f(resultCallback.rayFromWorld)
       resultCallback.rayToWorld.z = zMin
       resultCallback.closestHitFraction = 1f
       var lastRayResult = fromV.copy; lastRayResult.z = zMin
       while (distanceToSquaredXYPlane(fromV,lastRayResult) < distanceToCoverSquared) {
         resultCallback.rayFromWorld.x += direction.x
         resultCallback.rayFromWorld.y += direction.y
         resultCallback.rayToWorld.x = resultCallback.rayFromWorld.x
         resultCallback.rayToWorld.y = resultCallback.rayFromWorld.y
         collisionWrapper.collisionWorld.rayTest(resultCallback.rayFromWorld, resultCallback.rayToWorld, resultCallback)
         if (resultCallback.closestHitFraction != 1f) {
           // we hit something
           if (lastRayResult.z == zMin) {
             rayResult.append(lastRayResult.copy)
           }
           val triangle = models(0).getFaces(resultCallback.triangleIndex).toIndexedSeq.map(i => models(0).getVertices(i))
           val edgeHits = TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, fromV, direction)
           println("" + resultCallback.rayFromWorld + " -> " + edgeHits)
           if (edgeHits.size > 1) {
             println("Got two edgeHits, debug me")
           }
           edgeHits.foreach(edgeHit => {
             rayResult.append(edgeHit)
             resultCallback.rayFromWorld.x = edgeHit.x
             resultCallback.rayFromWorld.y = edgeHit.y
             lastRayResult.x = edgeHit.x; lastRayResult.y = edgeHit.y; lastRayResult.z = edgeHit.z;
           })
           resultCallback.closestHitFraction = 1f
           //rayResult.append(JBulletUtil.convertVector3fToVec3D(resultCallback.hitPointWorld))
         } else {
           // we hit nothing 
           if (rayResult.size>0){
             if (rayResult.last.z != zMin){
               rayResult.append(JBulletUtil.convertVector3fToVec3D(resultCallback.rayToWorld))
             }
           } else {
             rayResult.append(JBulletUtil.convertVector3fToVec3D(resultCallback.rayToWorld))
           }
           lastRayResult.x = resultCallback.rayToWorld.x; lastRayResult.y = resultCallback.rayToWorld.y; lastRayResult.z = resultCallback.rayToWorld.z;
         }
       }
       if (!rayResult.last.equals(lastRayResult)) {
         rayResult.append(lastRayResult.copy)
       }
     })

     rayResult
   }
   
   def cleanup = {
     //collisionWrapper.groundShape
     collisionWrapper.collisionWorld.destroy
   }
}

