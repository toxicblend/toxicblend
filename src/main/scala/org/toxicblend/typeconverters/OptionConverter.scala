package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlenderProtos.Option
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import scala.collection.JavaConversions._
import collection.mutable.HashMap

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