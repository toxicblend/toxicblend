package org.toxicblend.operations.zadjust

import org.toxicblend.CommandProcessorTrait
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
   */
  def getResonableScaling(aabb:AABB):Matrix4dE = {
    val extent:Vector3dE = new Vector3dE(aabb.aabbMax).subSelf(aabb.aabbMin)
    val negativeOrigin = new Vector3dE(aabb.aabbMax).addSelf(aabb.aabbMin).scaleSelf(-0.5)
    val maxExtent:Double = math.max(math.max(extent.x,extent.y),extent.z)
    assert(maxExtent>0)    
    val targetExtent = 300d // the new max extent, is it too large?
    val scale = targetExtent/maxExtent
    val matrix = new Matrix4dE(negativeOrigin, scale) 
    matrix
  }
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    if (inMessage.getModelsCount() < 2) {
      throw new ToxicblendException("At least two objects must be selected")
    }    
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("ZAdjustOperation: Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("ZAdjustOperation: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("ZAdjustOperation: Unrecognizable 'unitSystem' property value: " +  s ); None
    }
    val sampleStep:Float = (options.getOrElse("sampleStep", "0.1") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("ZAdjustOperation: unrecognizable 'sampleStep' property value: " +  s ); .1f
    } ) / 1000f // /1000 for conversion to mm
    val addDiff:Boolean = options.getOrElse("addDiff", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("ZAdjustOperation: Unrecognizable 'addDiff' property value: " +  s ); false
    }
    
    println(options)
    val epsilon = 0.000002f
    println("sampleStep="+ sampleStep + " epsilon=" + epsilon)
    //println("Input segment :" )
    //segments.foreach( s => println(s.mkString("\n")) )
    // models = all models found in inMessage except the first one
    
    val segments = {
      val s = Mesh3DConverter(inMessage.getModels(0),true).findContinuousLineSegments
      if (s._1.size > 0) throw new ToxicblendException("First object should only contain edges")
      s._2.filter( g => g.size <= 1).foreach(g => System.err.println("ZAdjustOperation:invalid input segment " + g))
      s._2.map( seg => seg.map(v => new Point3dE(v.x, v.y, v.z)))
    }
   
    val models = inMessage.getModelsList().tail.toIndexedSeq.map(i=>ByteBufferMeshConverter(i,true, unitScale))
    val aabbAllModels = new AABB(models.map(m=>m.aabb))
    val bulletScaling = getResonableScaling(aabbAllModels)
    val bulletScalingInverse = new Matrix4dE(bulletScaling).invertSelf
      
    // transform the input segments and models in place 
    segments.foreach(segment => segment.foreach( p=>bulletScaling.transform(p)))
    models.foreach(model => model.transformVertices(bulletScaling) )
    
    val result = {
      val facade = new BulletFacade(models)
      val collider = new Collider(facade, sampleStep, epsilon) 
      
      println("AABB max = " + facade.aabbAllModels.getMax )
      println("AABB min = " + facade.aabbAllModels.getMin )
  
      println("X size = " + (facade.aabbAllModels.getMax.x - facade.aabbAllModels.getMin.x) )
      println("Y size = " + (facade.aabbAllModels.getMax.y - facade.aabbAllModels.getMin.y) )
      println("Z size = " + (facade.aabbAllModels.getMax.z - facade.aabbAllModels.getMin.z) )
  
     
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
      /*
      val rv = new MutableList[IndexedSeq[Vec3D]]
      segments.filter( s => s.size > 1).foreach(segment => 
        rv += collider.doCollisionTests(segment, addDiff).map( s2 => {
          bulletScalingInverse.transform(s2)
          new Vec3D(s2.x.toFloat, s2.y.toFloat, s2.z.toFloat)
        })
      )*/
      facade.destroy
      result
    }
    println("Result:")
    result.foreach( s => println(s.mkString("\n")) )

    //collider.cleanup
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = new Mesh3DConverter("ray results") 
    result.foreach(s1 => s1.sliding(2).foreach(s2 => returnMeshConverter.addEdge(s2(0), s2(1))))
        
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}