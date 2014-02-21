package org.toxicblend.operations.boostsimplify

import org.toxicblend.UnitSystem
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Regex
import scala.collection.mutable.ArrayBuffer
import toxi.geom.{Vec3D,LineStrip3D}
import scala.collection.JavaConversions._
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify2D

import scala.collection.JavaConversions._

class BoostSimplifyOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("SimpleGcodeOperation: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("Unrecognizable 'unitSystem' property value: " +  s ); None
    }
 
    val simplifyLimit:Float = (options.getOrElse("simplifyLimit", "0.1") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("BoostSimplify: unrecognizable 'simplifyLimit' property value: " +  s ); .1f
    } ) / 1000f
    
    println("BoostSimplify::processInput simplifyLimit=" + simplifyLimit)
    
    val returnMessageBuilder = Message.newBuilder()
    val inverseMatrixes = new ArrayBuffer[Option[Matrix4x4Converter]]
    
    val models = inMessage.getModelsList().map(inModel => {
      (Mesh3DConverter(inModel,true), // Unit is now [meter]
      if (inModel.hasWorldOrientation()) {
        Option(Matrix4x4Converter(inModel.getWorldOrientation()))
      } else {
        None
      })
    })
       
    val result = models.map(model =>{
      //println("vertices=" + model._1.getVertices.mkString(","))
      //println("faces=" + model._1.getFaces.map(x=>x.mkString("(",",",")")).mkString(","))
      println("BoostSimplify::processInput bounds=" + model._1.getBounds)
      if (model._2.isDefined) {
        println("BoostSimplify::processInput world transformation=[" + model._2.get.matrix + "]")
      }
            
      val segments = model._1.findContinuousLineSegments
      val newMesh = new Mesh3DConverter("boost simplify"); 
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
    result.foreach(mesh3d => {
      val pbModel = 
      if (mesh3d._2.isDefined) {
        val worldOrientation = mesh3d._2.get
        val worldOrientationI = worldOrientation.matrix.copy; worldOrientationI.invert
        mesh3d._1.transformOne(worldOrientationI)
        
        val pbModel = mesh3d._1.toPBModel(None, None)
        //val v1 = new Vec3D(1,2,3)
        //val v2 = v1.copy(); worldOrientation.matrix.transformOne(v2)
        //val v3 = v2.copy(); worldOrientationI.transformOne(v3)
        //println("v1=" + v1 + " v2=" + v2 + " v3=" + v3)
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
