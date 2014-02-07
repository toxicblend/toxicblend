package org.toxicblend.operations.simplegcodeparse

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.UnitSystem
import org.toxicblend.util.Regex
import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec3D
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.protobuf.ToxicBlenderProtos.Message.Builder

import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.GCodeConverter
import org.toxicblend.typeconverters.Matrix4fConverter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify2D

import java.io.RandomAccessFile
import java.nio.channels.FileChannel
import java.nio.ByteBuffer
import java.io.IOException

class SimpleGcodeParseOperation extends CommandProcessorTrait {
    
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    println("ParseGcodeOperation::options=" + options)
    val returnMessageBuilder = Message.newBuilder()
    
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("Unrecognizable 'unitSystem' property value: " +  s ); None
    }
    if (unitIsMetric != UnitSystem.Metric) {
      System.err.println("ParseGcodeOperation::processInput only metric is supported for now");
    }
    
    if ( !options.contains("filename")) {
      System.err.println("ParseGcodeOperationNo filename given")
    } else {
      val filename = options("filename")
      try {
        SimpleGcodeParseOperation.readGcodeIntoBuilder(filename, options, returnMessageBuilder)
      } catch {
        case e: java.io.FileNotFoundException => System.err.println("ParseGcodeOperationNo file not found:\"" + filename + "\"")
        case e: Exception => throw e
      }
    }
    returnMessageBuilder
  }
}

object SimpleGcodeParseOperation {
  
  def readGcodeIntoBuilder(filename:String, options:OptionConverter, returnMessageBuilder:Builder) = {
    val parser = new GCodeParser
    try {      
      val reader = scala.io.Source.fromFile(filename)(scala.io.Codec.UTF8).getLines
      val filtered = parser.filterOutWhiteSpace(reader) 
      println("ParseGcodeOperation: filtered file size " + filtered.size)
      val parsed = parser.parseAll(parser.gCode,filtered)
      if (parsed.successful){
        GCodeConverter.writeGCode(parsed.get, options, returnMessageBuilder)
      } else {
        println("ParseGcodeOperation: failed to parse gcode. Filename: " + filename)
      }
    } catch {
      case exc:IOException => System.err.println(exc)
    }
  }
}