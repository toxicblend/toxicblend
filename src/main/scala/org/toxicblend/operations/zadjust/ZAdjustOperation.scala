package org.toxicblend.operations.zadjust

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Time.time
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.ByteBufferMeshConverter
import org.toxicblend.ToxicblendException
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.MutableList
import toxi.geom.ReadonlyVec3D
import toxi.geom.Matrix4x4
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import javax.vecmath.Vector3d
import javax.vecmath.Matrix4d
import toxi.geom.Vec3D
import com.bulletphysics.linearmath.AABB
import com.bulletphysics.linearmath.Vector3dE
import com.bulletphysics.linearmath.Point3dE
import com.bulletphysics.linearmath.Matrix4dE
import com.bulletphysics.linearmath.Tuple3dTrait

import scala.collection.JavaConversions._

class ZAdjustOperation extends CommandProcessorTrait {
  
  /**
   * Bullet (and jbullet) only works ok on objects of size 0.005 to 100 'units'
   * This method computes a linear scaling and translation matrix that converts the AABB to
   * values in this range 
   * returns a (scaling constant, scale & transform matrix)
   */
  def getResonableScaling(aabb:AABB):(Double,Matrix4dE) = {
    val extent:Vector3dE = new Vector3dE(aabb.aabbMax).subSelf(aabb.aabbMin)
    val negativeOrigin = new Vector3dE(aabb.aabbMax).addSelf(aabb.aabbMin).scaleSelf(-0.5)
    val maxExtent:Double = math.max(math.max(extent.x,extent.y),extent.z)
    assert(maxExtent>0)    
    val targetExtent = 300d // the new max extent, is it too large?
    val scale = targetExtent/maxExtent
    val matrix = new Matrix4dE(negativeOrigin, scale) 
    (scale,matrix)
  }
  
  def processInput(inMessage:Message, options:OptionConverter) = {
    
    val traceMsg = "ZAdjustOperation"

    if (inMessage.getModelsCount() < 2) {
      throw new ToxicblendException("At least two objects must be selected")
    }    
    val useMultiThreading = options.getMultiThreadingProperty(traceMsg)
    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitSystem = options.getUnitSystemProperty(traceMsg)
    val sampleStep = options.getFloatProperty("sampleStep", 0.1f, traceMsg)*unitScale/1000f  // convert from meter to mm
    val addDiff = options.getBooleanProperty("addDiff", false, traceMsg) 
    
    //println(options)
    //println("sampleStep="+ sampleStep)
    //println("Input segment :" )
    //segments.foreach( s => println(s.mkString("\n")) )
    // models = all models found in inMessage except the first one
    
    val segments = time(traceMsg + "findContinuousLineSegments: ",{
      val s = Mesh3DConverter(inMessage.getModels(0),true).findContinuousLineSegments
      if (s._1.size > 0) throw new ToxicblendException("First object should only contain edges")
      s._2.filter( g => g.size <= 1).foreach(g => System.err.println(traceMsg + ":invalid input segment " + g))
      s._2.map( seg => seg.map(v => new Point3dE(v.x, v.y, v.z)))
    })
   
    val models = time("Parse collision objects: ", 
        inMessage.getModelsList.tail.toIndexedSeq.map(i=>ByteBufferMeshConverter(i,true, unitScale)))
    val aabbAllModels = new AABB(models.map(m=>m.aabb))
    val (bulletScaling,bulletScalingM) = getResonableScaling(aabbAllModels)
    val bulletScalingInverse = new Matrix4dE(bulletScalingM).invertSelf
      
    // transform the input segments and models in place 
    segments.foreach(segment => segment.foreach( p=>bulletScalingM.transform(p)))
    models.foreach(model => model.transformVertices(bulletScalingM) )
    
    val result = time("Collision calculation time: ", {
      val facade = new BulletFacade(models)
      val collider = new Collider(facade, sampleStep*bulletScaling) 
      /*
      println("AABB max = " + facade.aabbAllModels.getMax )
      println("AABB min = " + facade.aabbAllModels.getMin )
  
      println("X size = " + (facade.aabbAllModels.getMax.x - facade.aabbAllModels.getMin.x) )
      println("Y size = " + (facade.aabbAllModels.getMax.y - facade.aabbAllModels.getMin.y) )
      println("Z size = " + (facade.aabbAllModels.getMax.z - facade.aabbAllModels.getMin.z) )
      */
      val result = if (useMultiThreading) {
        // this is wrong, each thread must have its own collider instance
        segments.filter( s => s.size > 1)./*par.*/map(segment => 
          collider.doCollisionTests(segment, addDiff).map( s2 => {
            bulletScalingInverse.transform(s2)
            new Vec3D(s2.x.toFloat, s2.y.toFloat, s2.z.toFloat)
          })
        )
      } else {
        segments.filter( s => s.size > 1).map(segment => 
          collider.doCollisionTests(segment, addDiff).map( s2 => {
            bulletScalingInverse.transform(s2)
            new Vec3D(s2.x.toFloat, s2.y.toFloat, s2.z.toFloat)
          })
        )
      }
      facade.destroy
      result
    })
    //println("Result:")
    //result.foreach( s => println(s.mkString("\n")) )

    //collider.cleanup
    time("Building resulting pBModel: ", {
      val returnMessageBuilder = Message.newBuilder()
      val returnMeshConverter = new Mesh3DConverter("ray results") 
      result.toSeq.foreach(s1 => s1.sliding(2).foreach(s2 => returnMeshConverter.addEdge(s2(0), s2(1))))
          
      returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
      returnMessageBuilder 
    })
  }
}