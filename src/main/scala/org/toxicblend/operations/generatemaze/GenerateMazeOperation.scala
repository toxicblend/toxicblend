package org.toxicblend.operations.generatemaze

import org.toxicblend.ToxicblendException
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import toxi.geom.ReadonlyVec3D
import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConversions._

class GenerateMazeOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    
    val options = OptionConverter(inMessage)
    val returnMessageBuilder = Message.newBuilder()
    val models = inMessage.getModelsList().map(inModel => {
      (Mesh3DConverter(inModel,false), // don't convert to world coordinates, instead keep the transform in the result
      if (inModel.hasWorldOrientation()) {
        Option(Matrix4x4Converter(inModel.getWorldOrientation()))
      } else {
        None
      })
    })
    val startPoint = options.getOrElse("startPoint", "RANDOM") match {
      case "CORNER" => StartPoint.CORNER
      case "CENTER" => StartPoint.CENTER
      case "RANDOM" => StartPoint.RANDOM
      case s:String => System.err.println("Unknown start point option: " +  s ); StartPoint.RANDOM
    }
    
    // Perform the generate maze on each model
    val result = models.map(model =>{    
      val mazegenerator = new MazeGenerator(model._1.getBounds)
      val segments = model._1.findContinuousLineSegments
      if (segments._2.size < 2) {
        throw new ToxicblendException("The object must only contain simple edges, try removing faces (while keeping the edges)")
      }
      segments._2.foreach(segment =>  {
        if (segment.size>=2) {
          mazegenerator.addWall(segment)
        }
      })
      returnMessageBuilder.addModels(mazegenerator.generateMaze(model._1.name + " maze", model._2, startPoint))
    })
    
    returnMessageBuilder
  }
}