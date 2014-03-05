package org.toxicblend.operations.simplegcodegenerator

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.ToxicblendException
import org.toxicblend.UnitSystem
import org.toxicblend.util.Regex
import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec3D
import org.toxicblend.util.Time
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.operations.simplegcodeparse.SimpleGcodeParseOperation

import scala.collection.JavaConversions._

class SimpleGcodeGeneratorOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    println("SimpleGcodeGeneratorOperation: options=" + options)
    val unitScaleProperty:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("SimpleGcodeOperation: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val gcodeProperties = {
      val unitIsMetricProperty = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
        case "METRIC" => UnitSystem.Metric
        case "NONE" => throw new ToxicblendException("SimpleGcodeGeneratorOperation:unitSystem=None but it's not supported"); None
        case "IMPERIAL" => throw new ToxicblendException("SimpleGcodeGeneratorOperation:unitSystem=IMPERIAL but it's not supported"); UnitSystem.Imperial
        case s:String => System.err.println("SimpleGcodeGeneratorOperation: Unrecognizable 'unitSystem' property value: " +  s ); None
      }
      val outFilename:String = options.getOrElse("outFilename", "gcode.ngc")
      val safeZProperty:Float = options.getOrElse("safeZ", "10").toFloat
      val g0FeedrateProperty:Float = options.getOrElse("g0Feedrate", "10").toFloat
      val g1FeedrateProperty:Float = options.getOrElse("g1Feedrate", "10f").toFloat
      val g1PlungeFeedrateProperty:Float = options.getOrElse("g1PlungeFeedrate", "10").toFloat
      val spindleSpeedProperty:Float = options.getOrElse("spindleSpeed", "10").toFloat
      val g64CommandProperty:String = options.getOrElse("g64Command", "G64 P0.02 Q0.02")
      val customEndCommandProperty:String = options.getOrElse("customEndCommand", "M101")      
      val stepDownProperty:Float = options.getOrElse("stepDown", "1").toFloat
      
      new GCodeSettings(outFilename=outFilename, safeZ=safeZProperty,g0Feedrate=g0FeedrateProperty, 
          g1Feedrate=g1FeedrateProperty,g1PlungeFeedrate=g1PlungeFeedrateProperty,
          spindleSpeed=spindleSpeedProperty,g64Command=g64CommandProperty,customEndCommand=customEndCommandProperty,stepDown=stepDownProperty)
    }
    Time.time("Building " + gcodeProperties.outFilename + " :",{ 
      // translate every vertex into world coordinates
      val models = inMessage.getModelsList().map(inModel => Mesh3DConverter(inModel,true,unitScaleProperty))
      val gCodeGenerator = new GCodeGenerator(gcodeProperties)
      val totalGCodes = gCodeGenerator.mesh3d2GCode(models(0))  // For now, only process the first model
      //println("totalGCodes: " + totalGCodes)
          
      val gcodeAsText = {
        var gcodeState:Option[GCodeState] = None
        totalGCodes.map(g => {
          val (s,t) = g.generateText(gcodeState, gcodeProperties)
          gcodeState = s
          t
        })  
      }
      gCodeGenerator.saveGCode(gcodeProperties.outFilename, gCodeGenerator.gHeader, gcodeAsText, gCodeGenerator.gFooter)
    })
    
    Time.time("Parsing " + gcodeProperties.outFilename + " :",{
      val returnMessageBuilder = Message.newBuilder()
      try {
        SimpleGcodeParseOperation.readGcodeIntoBuilder(gcodeProperties.outFilename, options, returnMessageBuilder)
      } catch {
        case e: java.io.FileNotFoundException => System.err.println("ParseGcodeOperationNo file not found:\"" + gcodeProperties.outFilename + "\""); throw e
        case e: Exception => throw e
      }
      returnMessageBuilder
    })
  }
}