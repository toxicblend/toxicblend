package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.ToxicblendException
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.MutableList
import toxi.geom.ReadonlyVec3D
import scala.collection.JavaConversions._
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem

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
      s._2
    }
    // models = all models found in inMessage except the first one
    val models = inMessage.getModelsList().tail.toIndexedSeq.map(i=>Mesh3DConverter(i,true))
    
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
    val jbc = new JBulletCollision(models, sampleStep, epsilon) 
    val result = new MutableList[IndexedSeq[ReadonlyVec3D]]
    if (addDiff) {
      segments.filter( s => s.size > 1).foreach(segment => {
        val collided = jbc.doCollisionTests(segment)
        result += jbc.adjustZLevel(segment,collided)
      })
    } else {
      segments.filter( s => s.size > 1).foreach(segment => result += jbc.doCollisionTests(segment).flatten)
    } 
    
    //println("Result:")
    //result.foreach( s => println(s.mkString("\n")) )

    jbc.cleanup
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = Mesh3DConverter(result.toList,"raytests")
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}