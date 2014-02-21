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
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.vecmath.Vector3f
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Plane
import toxi.geom.Ray3D
import toxi.geom.AABB

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
      // silently ignore edges and unconnected vertices
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

class JBulletCollision(val segments:IndexedSeq[IndexedSeq[ReadonlyVec3D]], val models:IndexedSeq[Mesh3DConverter], val sampleDelta:Float, val ε:Float) {
  // used when comparing squared lengthts
  // val εSqr = ε.toDouble*ε.toDouble
  
  class CollisionState extends TrianglePlaneIntersectionResult {
    val collisionPoint:Vec3D = new Vec3D
    
    @inline
    def setCollisionPoint(col:Vector3f) = {
      collisionPoint.x = col.x
      collisionPoint.y = col.y
      collisionPoint.z = col.z
    }
    
    @inline
    def setCollisionPoint(col:ReadonlyVec3D) = {
      collisionPoint.x = col.x
      collisionPoint.y = col.y
      collisionPoint.z = col.z
    }
  }
  
  /**
   * This is an effort to avoid having to create millions of Vec3D objects.
   */
  class SearchState(val rayFromWorld:Vector3f, val rayToWorld:Vector3f,val zMin:Float,val zMax:Float) extends RayResultCallback {
    var currentC:CollisionState = new CollisionState
    var hasPrevious:Boolean = false
    var previousC:CollisionState = new CollisionState
    val fromV:Vec3D = new Vec3D
    val toV:Vec3D = new Vec3D
    var distanceToCoverSqr = 0.0f
    var plane:Plane = new Plane
    //var directionNormalized:Vec3D = new Vec3D
    var directionDelta:Vec3D = new Vec3D
    var directionNormalized:Vec3D = new Vec3D
    val hitPointWorld = new Vector3f
    var triangleIndex:Int = -1
    var endOfSegmentReached = false
    
    /**
     * callback from jbullet on collision
     */
    override def addSingleResult(rayResult:LocalRayResult, normalInWorldSpace:Boolean):Float = {
      closestHitFraction = rayResult.hitFraction      
      VectorUtil.setInterpolate3(hitPointWorld, rayFromWorld, rayToWorld, rayResult.hitFraction)
      triangleIndex = rayResult.localShapeInfo.triangleIndex
      currentC.setCollisionPoint(hitPointWorld)
      rayResult.hitFraction
    }
      
    @inline
    def swapStates = {
      hasPrevious = true
      val tmp = previousC
      previousC = currentC
      currentC = tmp
      currentC.reset
    }
    
    def rayTest(collisionWorld:CollisionWorld):Boolean = {
      
      closestHitFraction = 1f
      collisionWorld.rayTest(rayFromWorld,rayToWorld,this)
      if ( closestHitFraction == 1.0f ){
        // we hit nothing, mark 'end of world' as collision point 
        currentC.setCollisionPoint(rayToWorld)
        //println("rayTest " + rayFromWorld + " -> " + rayToWorld + " -> air " + currentC.collisionPoint)
        false
      } else {
        //println("rayTest " + rayFromWorld + " -> " + rayToWorld + " -> hit " + currentC.collisionPoint)
        
        val triangle = models(0).getFaces(triangleIndex).toIndexedSeq.map(i => models(0).getVertices(i))
        TrianglePlaneIntersection.trianglePlaneIntersection(triangle, plane, currentC.collisionPoint, directionNormalized, currentC)
        
        if (currentC.hasForwardPoint){
          // test if the forward point overshot the segment 
          val distanceToTriangleIntersection = distanceToSquaredInXYPlane(fromV,currentC.forwardPoint) 
          if ( distanceToTriangleIntersection > distanceToCoverSqr ) {
            
            //val oldStr = "" + currentC.forwardPoint.x + ", " + currentC.forwardPoint.y + ", " + currentC.forwardPoint.z
            // sample at segment end
            conditionalJumpAheadToXY(toV)
            closestHitFraction = 1f 
            val oldX = currentC.collisionPoint.x
            val oldY = currentC.collisionPoint.y
            val oldZ = currentC.collisionPoint.z
            
            collisionWorld.rayTest(rayFromWorld,rayToWorld,this)
            
            currentC.forwardPoint.x = currentC.collisionPoint.x
            currentC.forwardPoint.y = currentC.collisionPoint.y
            currentC.forwardPoint.z = currentC.collisionPoint.z
            currentC.collisionPoint.x = oldX
            currentC.collisionPoint.y = oldY
            currentC.collisionPoint.z = oldZ
            currentC.hasForwardPoint = true
            //println("forward point overshot " + oldStr + " interpolated to " + currentC.forwardPoint.x + ", " + currentC.forwardPoint.y+ ", " + currentC.forwardPoint.z)
            /**
            if (currentC.hasRetroPoint ){
              val oldStr = "" + currentC.forwardPoint.x + ", " + currentC.forwardPoint.y + ", " + currentC.forwardPoint.z
              TrianglePlaneIntersection.interpolate3D(currentC.forwardPoint,currentC.retroPoint,currentC.forwardPoint, toV) 
              println("forward point overshot " + oldStr + " interpolated to " + currentC.forwardPoint.x + ", " + currentC.forwardPoint.y+ ", " + currentC.forwardPoint.z)
            }
            */
          }
        }
        true
      }
    }
    
    /**
     * resets the instance for reuse  
     */  
    def setSegment(newFromV:ReadonlyVec3D, newToV:ReadonlyVec3D) = {
      currentC.hasForwardPoint = false
      currentC.hasRetroPoint = false
      previousC.hasForwardPoint = false
      previousC.hasRetroPoint = false
      hasPrevious = false
      closestHitFraction = 1f
      endOfSegmentReached = false
      
      fromV.x = newFromV.x; fromV.y = newFromV.y; fromV.z = newFromV.z;
      toV.x = newToV.x; toV.y = newToV.y; toV.z = newToV.z;

      distanceToCoverSqr = distanceToSquaredInXYPlane(fromV,toV)
      plane = TrianglePlaneIntersection.segmentToZPlane(fromV,toV)
      directionNormalized = {
        val d = toV.sub(fromV)
        d.z = 0
        d.normalize
      } 
      directionDelta = directionNormalized.scale(sampleDelta)
      
      setRayOrigin(fromV)
      //println("setSegment: From= "+ fromV.x + "," + fromV.y + " to=" + toV.x + "," + toV.y + " Direction = " + direction.x + "," + direction.y)
    }
    
    @inline 
    def incrementPosition = {
      //val oldStr = "" + rayFromWorld.x + "," + rayFromWorld.y + " "
      //val atSegmentEnd = math.abs(rayFromWorld.x-toV.x) <= ε && math.abs(rayFromWorld.y-toV.y) <= ε
      //println(" toV.x         =" +  toV.x +         "  toV.y=        " + toV.y)
      //println(" rayFromWorld.x=" + rayFromWorld.x + " rayFromWorld.y=" + rayFromWorld.y)
      
      rayFromWorld.x += directionDelta.x
      rayFromWorld.y += directionDelta.y
      rayToWorld.x = rayFromWorld.x
      rayToWorld.y = rayFromWorld.y
      
      val distanceCoveredSqr = distanceToSquaredInXYPlane(fromV,rayFromWorld) 
      //println("distanceCoveredSoFar=" + distanceCoveredSoFar + " distanceToCoverSqr=" + distanceToCoverSqr + " :" + (distanceCoveredSoFar > distanceToCoverSqr)  + " atSegmentEnd:" + atSegmentEnd + " endOfSegmentReached:" + endOfSegmentReached)

      if (distanceCoveredSqr > distanceToCoverSqr){
        //if (!atSegmentEnd){
          //println("went to far, testing segment end")
          rayFromWorld.x = toV.x
          rayFromWorld.y = toV.y
          rayToWorld.x = toV.x
          rayToWorld.y = toV.y
        //}
      }
      //println("Incrementing position from " + oldStr + " to: " + rayFromWorld.x + "," + rayFromWorld.y + " should stop at " +toV.x + "," + toV.y )
    }
    
    /**
     * jumps the scan to new coordinates, but if the new coordinate exceeds the current segment it will be clipped
     * The method only touches X & Y coordinates
     */
    def conditionalJumpAheadToXY(point:Vec3D) = {
      val newDistanceSquared = distanceToSquaredInXYPlane(fromV,point)
      if (newDistanceSquared > distanceToCoverSqr){
        rayFromWorld.x = toV.x
        rayFromWorld.y = toV.y
        rayToWorld.x = toV.x
        rayToWorld.y = toV.y
        //println("jumping ahead to " + rayFromWorld + " (limited)")
      } else {
        rayFromWorld.x = point.x
        rayFromWorld.y = point.y
        rayToWorld.x = point.x
        rayToWorld.y = point.y
        //println("jumping ahead to " + rayFromWorld)
      }
    }
    
    /**
     * tests if we have reached the end of the segment. 
     * But allow to sample at the end coordinate for one iteration
     */
    @inline
    def isDone = {
      if (endOfSegmentReached) {
        // end of segment already sampled, we're done
        //println("isDone: endOfSegmentReached already seen, ending iteration at:" + rayFromWorld)
        true
      } else {
        val distanceCoveredSqr = distanceToSquaredInXYPlane(fromV,rayFromWorld) 
        //val diff = math.abs(distanceCoveredSoFar-distanceToCoverSqr)
        if ( distanceCoveredSqr >= distanceToCoverSqr) {
          // allow one more iteration at 'end of segment'
          endOfSegmentReached = true
          //println("isDone: setting endOfSegmentReached at: " + rayFromWorld + " distanceCoveredSqr=" + distanceCoveredSqr)
        }
        //println("isDone: continuing, distanceCoveredSqr = " + distanceCoveredSqr)
        false
      }
    }
    
    @inline
    def setRayOrigin(origin:ReadonlyVec3D) {
      rayFromWorld.x=origin.x
      rayFromWorld.y=origin.y
      rayFromWorld.z=zMax
      
      rayToWorld.x=origin.x
      rayToWorld.y=origin.y
      rayToWorld.z=zMin
    }
  }
  
  val collisionWrapper = new CollisionObjectWrapper(segments,models)
   val aabbAllModels = {
     if (models.size > 0){
       var aabb = models(0).getBounds.copy
       models.tail.foreach(b => aabb.union(b.getBounds))
       aabb
     } else {
       new AABB
     }
   }
   val zMin = aabbAllModels.getMin.z-1f
   val zMax = aabbAllModels.getMax.z+1f
   
   /**
    * returns the squared distance between two vertices in XY plane ( Z coordinate is ignored )
    */
   @inline
   def distanceToSquaredInXYPlane(v0:ReadonlyVec3D, v1:ReadonlyVec3D) = {
     val dx = (v0.x - v1.x)//.toDouble
     val dy = (v0.y - v1.y)//.toDouble
     dx*dx + dy*dy   
   }
   
   @inline
   def distanceToSquaredInXYPlane(v0:ReadonlyVec3D, v1:Vector3f) = {
     val dx = (v0.x - v1.x)//.toDouble
     val dy = (v0.y - v1.y)//.toDouble
     dx*dx + dy*dy   
   }
   
   def doRayTests(segments:IndexedSeq[ReadonlyVec3D]):IndexedSeq[ReadonlyVec3D] = {

     val aabb =  models(0).getBounds.copy()
     models.foreach(model => aabb.union(model.getBounds))
     
     println("BB min:" + aabbAllModels.getMin + " max: " + aabbAllModels.getMax + " zMin=" + zMin + " zMax=" + zMax)
     val rayResult = new ArrayBuffer[ReadonlyVec3D]
     
     @inline
     def addToRayResult(p:ReadonlyVec3D) = {
       if (rayResult.size<1 || rayResult.last != p){
         rayResult.append(p)
       }
     } 
     
     val searchState = new SearchState(new Vector3f(1, 1, zMax), new Vector3f(1, 1, zMin), zMin, zMax)
     
     segments.sliding(2,1).foreach(segment => {
       searchState.setSegment(segment(0), segment(1))
       
       while (!searchState.isDone) {
         if (searchState.rayTest(collisionWrapper.collisionWorld)) {
           // we hit something
           if (searchState.hasPrevious) {
             if (searchState.previousC.collisionPoint.z == zMin) {
               // we hit something but last time we didn't. So store that last miss
               val prevCopy = if (searchState.currentC.hasRetroPoint) {
                 searchState.currentC.retroPoint.copy               
               } else {
                 searchState.previousC.collisionPoint.copy
               }
               prevCopy.z = zMin
               //println("saving (modified) air hit: " + prevCopy)
               addToRayResult(prevCopy)
               if (searchState.currentC.hasRetroPoint) 
                 addToRayResult(searchState.currentC.retroPoint.copy)
               else 
                 System.err.println("No retro point, what to do???")
             }
           } else {
             //println("saving segment first hit : " + searchState.currentC.collisionPoint)
             addToRayResult(searchState.currentC.collisionPoint.copy)
           }
           
           if (searchState.currentC.hasForwardPoint) {
             //println("Adding front edge: " + searchState.currentC.forwardPoint)
             addToRayResult(searchState.currentC.forwardPoint.copy)
             //println("jumping ahead to " + searchState.currentC.forwardPoint )
             searchState.conditionalJumpAheadToXY(searchState.currentC.forwardPoint)
           } else {
             //println("Adding plain collision point: " + searchState.currentC.collisionPoint)
             addToRayResult(searchState.currentC.collisionPoint.copy)
           }
         } else {
           // we hit nothing
           if (rayResult.size>0){
             if (rayResult.last.z != zMin){
               // we have previous results with non-miss result
               val lastHit = rayResult.last.copy
               lastHit.z = zMin
               addToRayResult(lastHit)
             }
           } else {
             // the very first test was a dud, save it as an starting point
             addToRayResult(searchState.currentC.collisionPoint.copy)
           }
         }
         searchState.incrementPosition
         searchState.swapStates
       }
       // end of segment
       if (rayResult.size == 0 || 
            (searchState.hasPrevious && 
              (distanceToSquaredInXYPlane(searchState.toV,rayResult.last) > 
               distanceToSquaredInXYPlane(searchState.toV,searchState.previousC.collisionPoint)))) {
         // segment completed, save the last of the hits points 
         //println("done all tests, saving last hit" + searchState.previousC.collisionPoint)
         addToRayResult(searchState.previousC.collisionPoint.copy)
       }
     })
     rayResult
   }
   
   def cleanup = {
     //collisionWrapper.groundShape
     collisionWrapper.collisionWorld.destroy
   }
}

