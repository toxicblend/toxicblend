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
import org.toxicblend.util.Time.time
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
 * This operation subdivides the input edges into segments of @segmentDistance length. 
 * The edge will not be subdivided if it's already shorter than @segmentDistance.
 * The blender subdivide operations always splits the edges into a number of segments, regardless how short they are.
 */
class SubdivideEdgesOperation extends CommandProcessorTrait {
  
   def processInput(inMessage:Message, options:OptionConverter) = {
    
     val traceMsg = "SubdivideEdges"    
    // we are only using the first model as input
    val inModel = inMessage.getModelsList.get(0)
    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitSystem = options.getUnitSystemProperty(traceMsg)
    val segmentLength:Float = options.getFloatProperty("segmentLength", 1f, traceMsg)*unitScale/1000f  // convert from meter to mm
    
    val input = LineStripConverter(inModel,true)
    //println("input vertices:" + input.lineStrips.foldLeft(0)((b,a) => b+a.getVertices.size))
    //input.lineStrips.foreach(ls => println(ls.mkString(",")))
    val output = time(traceMsg + ":getDecimatedVertices calculation time: ", {
      val newStrips = input.lineStrips.map(ls => {
        //println("undecimated" + ls.getVertices.mkString(","))
        val decimated = ls.getDecimatedVertices(segmentLength)
        //println("decimated" + decimated.mkString(","))
        new LineStrip3D(decimated)  
      })
      LineStripConverter(newStrips,inModel.getName + "Subdivided Edges")
    })
    //println("output vertices:" + output.lineStrips.foldLeft(0)((b,a) => b+a.getVertices.size))
    //output.lineStrips.foreach(ls => println(ls.mkString(",")))
        
    time("Building resulting pBModel: ",{
      val messageBuilder = Message.newBuilder
      val pbModel = output.toPBModel(uniqueVertices=false)
	    //if (inModel.hasWorldOrientation()) {
	      // simply copy the world orientation
	    //  val mConverter = Matrix4x4Converter(inModel.getWorldOrientation)
	    //  pbModel.setWorldOrientation(mConverter.toPBModel)
	    //}
	    messageBuilder.addModels(pbModel)
    })
  }  
}