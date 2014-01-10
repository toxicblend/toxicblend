package org.toxicblend.operations.boostsimplify

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Regex
import scala.collection.mutable.ArrayBuffer
import toxi.geom.{Vec3D,LineStrip3D}
import scala.collection.JavaConversions._
import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Matrix4fConverter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify2D

import scala.collection.JavaConversions._

class BoostSimplify extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    
    val simplifyLimit:Float = options.getOrElse("simplifyLimit", "0.1") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("BoostSimplify: unrecognizable 'simplifyLimit' property value: " +  s ); .1f
    }
    
    val returnMessageBuilder = Message.newBuilder()
    val inverseMatrixes = new ArrayBuffer[Option[Matrix4fConverter]]
    
    val models = inMessage.getModelsList().map(inModel => {
      (Mesh3DConverter(inModel),
      if (inModel.hasWorldOrientation()) {
        Option(Matrix4fConverter(inModel.getWorldOrientation()))
      } else {
        None
      })
    })
       
    val result = models.map(model =>{
      println("vertexes=" + model._1.getVertexes.mkString(","))
      println("faces=" + model._1.getFaces.map(x=>x.mkString("(",",",")")).mkString(","))

      val segments = model._1.findLineSegments
      val newMesh = new Mesh3DConverter; 
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
    result.foreach(segment => {
      val pbModel = segment._1.toPBModel(None, None)
      if (segment._2.isDefined) pbModel.setWorldOrientation(segment._2.get.toPBModel)
      returnMessageBuilder.addModels(pbModel)
    })
    returnMessageBuilder
  }
}
