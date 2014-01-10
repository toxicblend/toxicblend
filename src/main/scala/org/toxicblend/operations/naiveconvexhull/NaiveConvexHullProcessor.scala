package org.toxicblend.operations.naiveconvexhull

import st0le.convexHull.NaiveConvexHull
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.Vec3D
import toxi.geom.AABB
import toxi.volume.{MeshLatticeBuilder,VolumetricSpace, VolumetricBrush, RoundBrush, BoxBrush, HashIsoSurface}
import toxi.util.datatypes.FloatRange
import scala.collection.JavaConversions._
import org.toxicblend.typeconverters.Matrix4fConverter
import org.toxicblend.typeconverters.Vertex2DPointCloudConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.geometry.ProjectionPlane.YZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XY_PLANE
import org.toxicblend.CommandProcessorTrait


class NaiveConvexHullProcessor extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    
    // we are only using the first model as input
    val inModel = inMessage.getModelsList().get(0)
    //println(inModel)
    val options = OptionConverter(inMessage)
    val ignoreAxis = options.getOrElse("ignoreAxleProperty", "IGNORE_Z") match {
      case "IGNORE_X" => YZ_PLANE
      case "IGNORE_Y" => XZ_PLANE
      case "IGNORE_Z" => XY_PLANE
      case s:String => System.err.println("Unknown projectionplane: " + s); XY_PLANE
    }
    
    //println(options.options)
    //println(ignoreAxis)
    
    val messageBuilder = Message.newBuilder()
    val pointCloud = Vertex2DPointCloudConverter(inModel, ignoreAxis, true).points.toList      
    val result = NaiveConvexHull.convexHull(pointCloud)
    val inverseMatrix = if (inModel.hasWorldOrientation()) {
      Option(Matrix4fConverter(inModel.getWorldOrientation()))
    } else {
      None
    }
    val pbModel = Vertex2DPointCloudConverter(result, ignoreAxis, inModel.getName + " Convex Hull").toPBModel(true, inverseMatrix)
    
    //pbModel.setName("Convex Hull")
    messageBuilder.addModels(pbModel)
    //println(pbModel.build)
    messageBuilder
  }  
}