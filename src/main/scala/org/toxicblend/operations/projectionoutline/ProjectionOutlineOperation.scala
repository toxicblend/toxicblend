package org.toxicblend.operations.projectionoutline

import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import toxi.geom.Vec3D
import org.toxicblend.geometry.ProjectionPlane.YZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XY_PLANE
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.typeconverters.Mesh2DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Time.time
import akka.actor.ActorSystem

class ProjectionOutlineOperation(private val actorSystem: ActorSystem) extends CommandProcessorTrait {

  def processInput(inMessage: Message, options: OptionConverter) = {
    val traceMsg = "ProjectionOutlineOperation"

    // we are only using the first model as input
    val inModel = inMessage.getModelsList.get(0)
    //println(inModel)
    val projectionPlane = options.getOrElse("projectionPlane", "XY_PLANE") match {
      case "YZ_PLANE" => YZ_PLANE
      case "XZ_PLANE" => XZ_PLANE
      case "XY_PLANE" => XY_PLANE
      case s: String => System.err.println("ProjectionOutlineOperation::Unknown projection: " + s); XY_PLANE
    }

    val useMultiThreading = options.getMultiThreadingProperty(traceMsg, true)

    val returnMessageBuilder = Message.newBuilder()
    val result = Mesh2DConverter(inModel, projectionPlane, true)

    time("projectionOutline: ", result.mesh2d.projectionOutline(actorSystem, useMultiThreading))

    val inverseMatrix = if (inModel.hasWorldOrientation()) {
      Option(Matrix4x4Converter(inModel.getWorldOrientation()))
    } else {
      None
    }
    time("Building resulting pBModel: ", {
      val returnPbModel = result.toPBModel(true, inverseMatrix)
      returnPbModel.setName(inModel.getName + " Projection Outline")
      returnMessageBuilder.addModels(returnPbModel)
    })
  }
}