package org.toxicblend.operations.projectionoutline

import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import toxi.geom.Vec3D
import org.toxicblend.geometry.ProjectionPlane.YZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XY_PLANE
import org.toxicblend.typeconverters.Matrix4fConverter
import org.toxicblend.typeconverters.Mesh2DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Time

class ProjectionOutlineProcessor extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    
    // we are only using the first model as input
    val inModel = inMessage.getModelsList().get(0)
    //println(inModel)
    val options = OptionConverter(inMessage)
    val projectionPlane = options.getOrElse("projectionPlane", "XY_PLANE") match {
      case "YZ_PLANE" => YZ_PLANE
      case "XZ_PLANE" => XZ_PLANE
      case "XY_PLANE" => XY_PLANE
      case s:String => System.err.println("Unknown projection: " +  s ); XY_PLANE
    }
    val multiThreadProperty = options.getOrElse("multiThreadProperty", "FALSE") match {
      case "FALSE" => false
      case "TRUE" => true
      case s:String => System.err.println("ProjectionOutlineProcessor: Unknown multiThreadProperty property value: " +  s ); false
    }
    
    println(options.options)
    
    val returnMessageBuilder = Message.newBuilder()
    val result = Mesh2DConverter(inModel, projectionPlane, true)
    
    Time.time(result.mesh2d.projectionOutline(multiThreadProperty))
    
    val inverseMatrix = if (inModel.hasWorldOrientation()) {
      Option(Matrix4fConverter(inModel.getWorldOrientation()))
    } else {
      None
    }
    
    val returnPbModel = result.toPBModel(true, inverseMatrix)    
    returnPbModel.setName(inModel.getName + " Projection Outline")
    returnMessageBuilder.addModels(returnPbModel)
    returnMessageBuilder
  }  
}