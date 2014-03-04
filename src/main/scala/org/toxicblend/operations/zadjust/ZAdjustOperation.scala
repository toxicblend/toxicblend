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
import scala.collection.JavaConversions._
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import toxi.geom.Vec3D

class ZAdjustOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    if (inMessage.getModelsCount() < 2) {
      throw new ToxicblendException("At least two objects must be selected")
    }
    val segments = {
      val s = Mesh3DConverter(inMessage.getModels(0),true).findContinuousLineSegments
      if (s._1.size > 0) throw new ToxicblendException("First object should only contain edges")
      s._2.filter( g => g.size <= 1).foreach(g => System.err.println("ZAdjustOperation:invalid input segment " + g))
      s._2.map( seg => seg.map(v => new Point3d(v.x, v.y, v.z)))
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
    
    // models = all models found in inMessage except the first one
    val models = inMessage.getModelsList().tail.toIndexedSeq.map(i=>ByteBufferMeshConverter(i,true, unitScale))
    
    println(options)
    val epsilon = 0.000002f
    println("sampleStep="+ sampleStep + " epsilon=" + epsilon)
    //println("Input segment :" )
    //segments.foreach( s => println(s.mkString("\n")) )
    val collisionWrapper = new BulletFacade(models)
    val collider = new Collider(collisionWrapper, sampleStep, epsilon) 
    
    println("AABB max = " + collider.collisionWrapper.aabbAllModels.getMax )
    println("AABB min = " + collider.collisionWrapper.aabbAllModels.getMin )

    println("X size = " + (collider.collisionWrapper.aabbAllModels.getMax.x - collider.collisionWrapper.aabbAllModels.getMin.x) )
    println("Y size = " + (collider.collisionWrapper.aabbAllModels.getMax.y - collider.collisionWrapper.aabbAllModels.getMin.y) )
    println("Z size = " + (collider.collisionWrapper.aabbAllModels.getMax.z - collider.collisionWrapper.aabbAllModels.getMin.z) )

    val result = new MutableList[IndexedSeq[Vec3D]]
    if (addDiff) {
      segments.filter( s => s.size > 1).foreach(segment => {
        val collided = collider.doCollisionTests(segment)
        result += collider.adjustZLevel(segment.map(v => new Vec3D(v.x.toFloat,v.y.toFloat,v.z.toFloat)),collided)
      })
    } else {
      segments.filter( s => s.size > 1).foreach(segment => result += collider.doCollisionTests(segment).flatten)
      //println(result)
    } 
    
    //println("Result:")
    //result.foreach( s => println(s.mkString("\n")) )

    //collider.cleanup
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = new Mesh3DConverter("rayresults") 
    result.foreach(s1 => s1.sliding(2).foreach(s2 => returnMeshConverter.addEdge(s2(0), s2(1))))
        
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}