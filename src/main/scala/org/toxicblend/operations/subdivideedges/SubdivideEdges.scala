package org.toxicblend.operations.subdivideedges

import org.toxicblend.ToxicblendException
import org.toxicblend.util.Regex
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.Vec3D
import toxi.geom.mesh.LaplacianSmooth
import toxi.geom.AABB
import toxi.geom.Matrix4x4
import toxi.geom.LineStrip3D
import toxi.volume.{MeshLatticeBuilder,VolumetricSpace, VolumetricBrush, RoundBrush, BoxBrush, HashIsoSurface}
import toxi.util.datatypes.FloatRange
import scala.collection.JavaConversions._
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.typeconverters.LineStripConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.UnitSystem
import org.toxicblend.geometry.Matrix4x4Implicit._

/**
 * This operation simply subdivides the input edges into segments of @segmentDistance length
 */
class SubdivideEdges extends CommandProcessorTrait {
  
   def processInput(inMessage:Message, options:OptionConverter) = {
    
    // we are only using the first model as input
    val inModel = inMessage.getModelsList().get(0) 
    
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("BoostSimplifyOperation: unrecognizable 'unitScale' property value: " +  s); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => throw new ToxicblendException("BoostSimplifyOperation: unitSystem=None but it's not supported"); None
      case "IMPERIAL" => throw new ToxicblendException("BoostSimplifyOperation:u nitSystem=IMPERIAL but it's not supported"); UnitSystem.Imperial
      case s:String => System.err.println("BoostSimplifyOperation: Unrecognizable 'unitSystem' property value: " +  s ); None
    }
    val segmentLength:Float = options.getOrElse("segmentLength", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => unitScale*limit.toFloat / 1000f
      case s:String => System.err.println("SubdivideEdges: unrecognizable 'segmentLength' property value: " +  s ); unitScale/1000f
    }
    
    val input = LineStripConverter(inModel,true)
    println("input vertices:" + input.lineStrips.foldLeft(0)((b,a) => b+a.getVertices.size))
    //input.lineStrips.foreach(ls => println(ls.mkString(",")))
    val output = {
      val newStrips = input.lineStrips.map(ls => {
        //println("undecimated" + ls.getVertices.mkString(","))
        val decimated = ls.getDecimatedVertices(segmentLength)
        //println("decimated" + decimated.mkString(","))
        new LineStrip3D(decimated)  
      })
      LineStripConverter(newStrips,"Subdivided Edges")
    }
    println("output vertices:" + output.lineStrips.foldLeft(0)((b,a) => b+a.getVertices.size))
    //output.lineStrips.foreach(ls => println(ls.mkString(",")))
        
    {
      val messageBuilder = Message.newBuilder
      val pbModel = output.toPBModel(uniqueVertices=false)
	    //if (inModel.hasWorldOrientation()) {
	      // simply copy the world orientation
	    //  val mConverter = Matrix4x4Converter(inModel.getWorldOrientation)
	    //  pbModel.setWorldOrientation(mConverter.toPBModel)
	    //}
	    messageBuilder.addModels(pbModel)
	    //messageBuilder
    }
  }  
}