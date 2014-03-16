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
import toxi.geom.Polygon2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class Offset2dShapeOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    
    val options = OptionConverter(inMessage)
        
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => System.err.println("Offset2dShapeOperation: useMultiThreading=True but it's not implemented yet"); true
      case "FALSE" => false
      case s:String => System.err.println("Offset2dShapeOperation: Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val useToOutline = options.getOrElse("useToOutline", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("Unrecognizable 'useToOutline' property value: " +  s ); false
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
               
    // Convert model vertices to world coordinates so that the offset unit makes sense
    val models = inMessage.getModelsList.map(inModel => {
      (Mesh3DConverter(inModel,true), // Unit is now [meter]
      if (inModel.hasWorldOrientation()) {
        Option(Matrix4x4Converter(inModel.getWorldOrientation))
      } else {
        None
      })
    })
    
    val returnPolygons = new ArrayBuffer[Polygon2DConverter]
    // Perform the simplify operation
    Time.time("FindContinuousLineSegments calculation time: ", models.foreach(model =>{      
     val segments = model._1.findContinuousLineSegments._2
     
     val (polygons, transforms) = Polygon2DConverter.toPolygon2D(segments)
     val newMesh = new Polygon2DConverter(polygons, transforms, "Offset shapes"); 
     returnPolygons.append(newMesh)
    }))
           
    // Perform the offset operation
    Time.time("Executing offsetShape : ", returnPolygons.foreach(pc => pc.polygons.foreach(p=>p.offsetShape(offset))))
    
    if (useToOutline) {
      // Perform the toOutline operation
      Time.time("Executing toOutline : ", returnPolygons.foreach(pc => pc.polygons.foreach(p=>p.toOutline)))
    }
        
    Time.time("Building resulting pBModel: ",{
      val returnMessageBuilder = Message.newBuilder
      returnPolygons.foreach(pc => returnMessageBuilder.addModels(pc.toPBModel(None)))
      returnMessageBuilder
    })
  }
}
