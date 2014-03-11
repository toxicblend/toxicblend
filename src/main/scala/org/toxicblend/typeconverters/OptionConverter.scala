package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Option
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import scala.collection.JavaConversions._
import collection.mutable.HashMap
import org.toxicblend.util.Regex
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D

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
   * TODO: very imperative
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
  def getCursorPos:ReadonlyVec3D = {
    val cursorPosX:Float = options.getOrElse("cursorPosX", "0.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("OptionConverter: unrecognizable 'cursorPosX' property value: " +  s ); 0f
    }
    val cursorPosY:Float = options.getOrElse("cursorPosY", "0.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("OptionConverter: unrecognizable 'cursorPosY' property value: " +  s ); 0f
    }
    val cursorPosZ:Float = options.getOrElse("cursorPosZ", "0.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("OptionConverter: unrecognizable 'cursorPosZ' property value: " +  s ); 0f
    }
    new Vec3D(cursorPosX,cursorPosY,cursorPosZ)
  }
  
  override def toString() = options.toString
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