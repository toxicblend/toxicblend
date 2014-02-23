package org.toxicblend.operations.boostsimplify

import org.toxicblend.UnitSystem
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Regex
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import toxi.geom.Vec3D
import toxi.geom.LineStrip3D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class BoostSimplifyOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => System.err.println("BoostSimplifyOperation:useMultiThreading=True but it's not implemented yet"); true
      case "FALSE" => false
      case s:String => System.err.println("Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("SimpleGcodeOperation: unrecognizable 'unitScale' property value: " +  s); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("Unrecognizable 'unitSystem' property value: " +  s); None
    } 
    val simplifyLimit:Float = (options.getOrElse("simplifyLimit", "0.1") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("BoostSimplify: unrecognizable 'simplifyLimit' property value: " +  s); .1f
    } ) / 1000f  // convert from meter to mm
        
    val returnMessageBuilder = Message.newBuilder()
    val inverseMatrixes = new ArrayBuffer[Option[Matrix4x4Converter]]
    
    
    // Convert model vertices to world coordinates so that the simplify scaling makes sense
    val models = inMessage.getModelsList().map(inModel => {
      (Mesh3DConverter(inModel,true), // Unit is now [meter]
      if (inModel.hasWorldOrientation()) {
        Option(Matrix4x4Converter(inModel.getWorldOrientation()))
      } else {
        None
      })
    })
    
    // Perform the simplify operation
    val result = models.map(model =>{      
      val segments = model._1.findContinuousLineSegments
      val newMesh = new Mesh3DConverter(model._1.name + " boost simplify"); 
      segments._1.foreach(ngon => newMesh.addFace(ngon))
      segments._2.foreach(segment =>  {
        if (segment.size>2) {
          val simplifiedSegment = simplify3D(segment, simplifyLimit)
          newMesh.addMultipleEdges(simplifiedSegment)
        } else {
          newMesh.addMultipleEdges(segment)
        }
      })
      (newMesh,model._2)
    })
    
    // Convert the coordinate system back again (world orientation matrix.inverse)
    result.foreach(mesh3d => {
      val pbModel = 
      if (mesh3d._2.isDefined) {
        val worldOrientation = mesh3d._2.get
        val worldOrientationI = worldOrientation.matrix.copy; worldOrientationI.invert
        mesh3d._1.transformOne(worldOrientationI)
        
        val pbModel = mesh3d._1.toPBModel(None, None)
        pbModel.setWorldOrientation(worldOrientation.toPBModel)
        pbModel
      } else {
        mesh3d._1.toPBModel(None, None)
      }
      returnMessageBuilder.addModels(pbModel)
    })
    returnMessageBuilder
  }
}
