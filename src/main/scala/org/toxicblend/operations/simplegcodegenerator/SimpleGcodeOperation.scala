package org.toxicblend.operations.simplegcodegenerator

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
import  org.toxicblend.operations.gcodeparse.ParseGcodeOperation

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
    val outFilename:String = options.getOrElse("outFilename", "gcode.ngc")
    
    if (unitIsMetric != UnitSystem.Metric) {
      System.err.println("SimpleGcodeOperation::processInput only metric is supported for now");
    }
    
    // translate every vertex into world coordinates
    val models = inMessage.getModelsList().map(inModel => Mesh3DConverter(inModel,true))
    
    val totalGCodes = GenerateGCode.mesh3d2GCode(models(0))  // For now, only process the first model
    GenerateGCode.saveGCode(outFilename, GenerateGCode.gHeader, totalGCodes.map(g => g.generateText(GenerateGCode.gCodeProperties)), GenerateGCode.gFooter)
    
    println(options)
    val returnMessageBuilder = Message.newBuilder()
    try {
      ParseGcodeOperation.readGcode(outFilename, options, returnMessageBuilder)
    } catch {
      case e: java.io.FileNotFoundException => System.err.println("ParseGcodeOperationNo file not found:\"" + outFilename + "\"")
      case e: Exception => throw e
    }
      
    returnMessageBuilder
  }
}