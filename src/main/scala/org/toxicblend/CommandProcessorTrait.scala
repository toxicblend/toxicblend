package org.toxicblend

import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Message.{Builder => MessageBuilder}
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.util.Regex
import org.toxicblend.typeconverters.Matrix4x4Converter

trait CommandProcessorTrait {
  def processInput(inMessage:Message):MessageBuilder
  
  protected def getUnitSystem(options:OptionConverter, traceMsg:String) = {
     options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => throw new ToxicblendException(traceMsg +": unitSystem=None but it's not supported"); None
      case "IMPERIAL" => throw new ToxicblendException(traceMsg + ":unitSystem=IMPERIAL but it's not supported"); UnitSystem.Imperial
      case s:String => System.err.println(traceMsg + ": Unrecognizable 'unitSystem' property value: " +  s ); None
    }
  }
  
  protected def getWorldModel(inModel:Model) = {
    if (inModel.hasWorldOrientation) {
      Option(Matrix4x4Converter(inModel.getWorldOrientation))
    } else {
      None
    }
  }
  
  protected def getBooleanProperty(options:OptionConverter, propertyName:String, default:Boolean, traceMsg:String) = {
    options.getOrElse(propertyName, default.toString).toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println(traceMsg + ": Unrecognizable '"+propertyName+"' property value: " +  s ); default
    }
  }
  
  protected def getFloatProperty(options:OptionConverter, propertyName:String, default:Float, traceMsg:String) = {
    options.getOrElse(propertyName, default.toString) match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println(traceMsg + ": unrecognizable '"+ propertyName + "' property value: " +  s); default
    }
  }

  protected def getMultiThreadingProperty(options:OptionConverter, traceMsg:String) = {
    getBooleanProperty(options, "useMultiThreading", false, traceMsg)
  }
  
  protected def getUnitScaleProperty(options:OptionConverter, traceMsg:String) = {
    getFloatProperty(options,"unitScale",1f,traceMsg)
  }
}