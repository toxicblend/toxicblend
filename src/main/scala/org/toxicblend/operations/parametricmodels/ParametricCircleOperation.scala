package org.toxicblend.operations.parametricmodels

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D

/**
 * A 'draw custom geometry' example 
 */
class ParametricCircleOperation extends CommandProcessorTrait {
  
  def draw(options:OptionConverter):Mesh3DConverter = {
    val rv = new Mesh3DConverter
    val circle = new Array[ReadonlyVec3D](128)
    val deltaDegree = (2d*Math.PI) / circle.size
    var degree = 0d
    (0 until circle.size).foreach(i => {
      circle(i) = new Vec3D(Math.cos(degree).toFloat, Math.sin(degree).toFloat, 0)
      degree += deltaDegree
    })
    circle.sliding(2).foreach(l => rv.addEdge(l(0),l(1)))
    rv.addEdge(circle.last,circle(0))
    
    var r = 0.1f
    val deltaR = 0.1f
    (0 until 10).foreach(l => {
      val circum = 2.*Math.PI*r
      val steps = ((0.1d/circum)*circle.size).toInt
      (0 until (circle.size, steps) ).foreach( s => {
         rv.addEdge(circle(s).scale(r-deltaR),circle(s).scale(r))
      })
      circle.sliding(2).foreach(l => rv.addEdge(l(0).scale(r),l(1).scale(r)))
      rv.addEdge(circle.last.scale(r),circle(0).scale(r))
      r += deltaR
    })
    rv
  }
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage) 
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("ParametricCircleOperation: Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("ParametricCircleOperation: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("ParametricCircleOperation: Unrecognizable 'unitSystem' property value: " +  s ); None
    } 
    println("ParametricCircleOperation options: " + options)
    
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = draw(options)
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}