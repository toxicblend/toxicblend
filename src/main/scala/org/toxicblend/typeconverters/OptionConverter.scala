package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Option
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import scala.collection.JavaConversions._
import collection.mutable.HashMap
import org.toxicblend.util.Regex
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import org.toxicblend.ToxicblendException
import org.toxicblend.UnitSystem

class OptionConverter(val options:collection.mutable.Map[String,String]){
  
  def this() =  {
    this(new HashMap[String,String]())  
  }
  
  def add(key:String, value:String) = {
    options += Tuple2(key,value)
    this
  }
  
  /**
   * Will add the options to the message
   */  
  def toPBModel(message:Message.Builder) = {
    options.foreach( o => {
      val builder = Option.newBuilder()
      builder.setKey(o._1)
      builder.setValue(o._2)
      message.addOptions(builder)
    })
    message
  }
  
  def apply(key:String):String = {
    options(key)
  }
  
  def contains(key:String):Boolean = {
    options.contains(key)
  }
  
  def getOrElse(key:String,default:String):String = {
    options.getOrElse(key,default)
  }
  
  /**
   * returns the cursor position stored in the cursorPosX, cursorPosY and cursorPosZ property values
   */
  def getCursorPosProperty(traceMsg:String):ReadonlyVec3D = {
    val cursorPosX = getFloatProperty("cursorPosX", 0f, traceMsg) 
    val cursorPosY = getFloatProperty("cursorPosY", 0f, traceMsg)
    val cursorPosZ = getFloatProperty("cursorPosZ", 0f, traceMsg)
    new Vec3D(cursorPosX,cursorPosY,cursorPosZ)
  }
  
  override def toString() = options.toString
  
  def getUnitSystemProperty(traceMsg:String) = 
    getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => throw new ToxicblendException(traceMsg +": unitSystem=None but it's not supported"); None
      case "IMPERIAL" => throw new ToxicblendException(traceMsg + ":unitSystem=IMPERIAL but it's not supported"); UnitSystem.Imperial
      case s:String => System.err.println(traceMsg + ": Unrecognizable 'unitSystem' property value: " +  s ); None
    }

  def getBooleanProperty(propertyName:String, default:Boolean, traceMsg:String) = 
    getOrElse(propertyName, default.toString).toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println(traceMsg + ": Unrecognizable '"+propertyName+"' property value: " +  s ); default
    }
  
  def getFloatProperty(propertyName:String, default:Float, traceMsg:String) = 
    getOrElse(propertyName, default.toString) match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println(traceMsg + ": unrecognizable '"+ propertyName + "' property value: " +  s); default
    }
  
  def getIntProperty(propertyName:String, default:Int, traceMsg:String) = 
    getOrElse(propertyName, default.toString) match {
      case Regex.INT_REGEX(limit) => limit.toInt
      case s:String => System.err.println(traceMsg + ": unrecognizable '"+ propertyName + "' property value: " +  s); default
    }
  
  def getStringProperty(propertyName:String, default:String) = getOrElse(propertyName, default)
 
  def getMultiThreadingProperty(traceMsg:String, default:Boolean=false) = getBooleanProperty("useMultiThreading", default, traceMsg)
  
  def getUnitScaleProperty(traceMsg:String, default:Float=1f) = getFloatProperty("unitScale", default, traceMsg)
  
}

object OptionConverter {
  
  def apply(inPBMessage:Message):OptionConverter = {
    val map = new HashMap[String,String]
    //println(" got " + inPBMessage.getOptionsList().length + " options ")
    inPBMessage.getOptionsList().foreach(o=>{
      //println("" + o.getKey + " -> " + o.getValue)
      map(o.getKey) = o.getValue
    })
    new OptionConverter(map)
  }
}