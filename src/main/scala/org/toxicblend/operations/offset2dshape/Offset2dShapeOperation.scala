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
      case "TRUE" => true
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
    
    val returnPolygons = Time.time("FindPlanes calculation time: ", {
      def findSequenceOfPolygons( model:(Mesh3DConverter,Option[Matrix4x4Converter]) ) = {
        val segments = model._1.findContinuousLineSegments._2.filter(seq => seq.size>2)
        if (segments.size ==0) {
          println("No edge sequence found in input model.")  
        }
	      val pt = Polygon2DConverter.toPolygon2D(segments)
	      new Polygon2DConverter(pt.map(p => p._1), pt.map(t => t._2), "Offset shapes")
      }
      if (useMultiThreading) {
        models.par.map(model => findSequenceOfPolygons(model))
      } else {
        models.map(model => findSequenceOfPolygons(model))
      }
    })
           
    Time.time("Executing offsetShape : ", returnPolygons.foreach(pc => pc.polygons.foreach(p=>p.offsetShape(offset) )))
    
    if (useToOutline) {
	    Time.time("Executing toOutline : ", returnPolygons.foreach(pc => pc.polygons.foreach(p=>p.toOutline)))
    }
        
    Time.time("Building resulting pBModel: ",{
      val returnMessageBuilder = Message.newBuilder
      if (useMultiThreading){
        // convert the .par sequence back to a normal sequence
        returnPolygons.toIndexedSeq.foreach(pc => returnMessageBuilder.addModels(pc.toPBModel(None)))
      } else {
        returnPolygons.foreach(pc => returnMessageBuilder.addModels(pc.toPBModel(None)))
      }
      returnMessageBuilder
    })
  }
}
