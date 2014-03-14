package org.toxicblend.operations.offset2dshape

import org.toxicblend.ToxicblendException
import org.toxicblend.UnitSystem
import org.toxicblend.util.Time
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.geometry.ProjectionPlane.YZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XY_PLANE
import org.toxicblend.typeconverters.Rings2DConverter
import org.toxicblend.util.Regex
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Polygon2DConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import toxi.geom.Vec3D
import toxi.geom.LineStrip3D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class Offset2dShapeOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    // we are only using the first model as input
    val inModel = inMessage.getModelsList().get(0)
    
    val options = OptionConverter(inMessage)
    
    val projectionPlane = options.getOrElse("projectionPlane", "None") match {
      case "YZ_PLANE" => YZ_PLANE
      case "XZ_PLANE" => XZ_PLANE
      case "XY_PLANE" => XY_PLANE
      case _ => throw new IllegalArgumentException("No projection plane specified")
      //case s:String => System.err.println("Unknown projection: " +  s ); None
    }
    
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => System.err.println("Offset2dShapeOperation=True but it's not implemented yet"); true
      case "FALSE" => false
      case s:String => System.err.println("Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("Offset2dShapeOperation: unrecognizable 'unitScale' property value: " +  s); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => throw new ToxicblendException("Offset2dShapeOperation=None but it's not supported"); None
      case "IMPERIAL" => throw new ToxicblendException("Offset2dShapeOperation=IMPERIAL but it's not supported"); UnitSystem.Imperial
      case s:String => System.err.println("Offset2dShapeOperation: Unrecognizable 'unitSystem' property value: " +  s ); None
    }
    val offset:Float = (options.getOrElse("offset", "0.1") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("Offset2dShapeOperation: unrecognizable 'simplifyLimit' property value: " +  s); .1f
    } ) / 1000f  // convert from meter to mm
               
    val worldOrientationMatrix = if (inModel.hasWorldOrientation()) {
      Option(Matrix4x4Converter(inModel.getWorldOrientation))
    } else {
      None
    }
    
    // Convert model vertices to world coordinates so that the offset value has correct unit
    val rings2D = Polygon2DConverter(Rings2DConverter(inModel, projectionPlane, applyWorldTransform=true))
    
    // Perform the offset operation
    Time.time("Executing offsetShape : ", rings2D.polygons.foreach(p => p.offsetShape(offset)))
    
    Time.time("Building resulting pBModel: ",{
      val returnMessageBuilder = Message.newBuilder
       returnMessageBuilder.addModels(rings2D.toPBModel(worldOrientationMatrix))
      returnMessageBuilder
    })
  }
}
