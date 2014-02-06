package org.toxicblend.operations.simplegcode

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.UnitSystem
import org.toxicblend.util.Regex
import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec3D
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Matrix4fConverter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify2D

import scala.collection.JavaConversions._

class SimpleGcodeOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("SimpleGcodeOperation: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("Unrecognizable 'unitSystem' property value: " +  s ); None
    }
    if (unitIsMetric != UnitSystem.Metric) {
      System.err.println("SimpleGcodeOperation::processInput only metric is supported for now");
    }
    
    // translate every vertex into world coordinates
    val models = inMessage.getModelsList().map(inModel => Mesh3DConverter(inModel,true))
    
    println(options)
    val returnMessageBuilder = Message.newBuilder()
    returnMessageBuilder
  }
}