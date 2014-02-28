package org.toxicblend.operations.boostmedianaxis

import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.geometry.ProjectionPlane.YZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XY_PLANE
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.typeconverters.Rings2DConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.util.Regex
import org.toxicblend.CommandProcessorTrait
import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable.ArrayBuffer
import org.toxicblend.util.Time
import toxi.geom.ReadonlyVec3D

class MedianAxisOperation extends CommandProcessorTrait {
  
  protected def manageInput(inMessage:Message) = {
    // we are only using the first model as input
    val inModel = inMessage.getModelsList().get(0)
    val objectName = inModel.getName
    val options = OptionConverter(inMessage)
    println(options)
    
    val projectionPlane = options.getOrElse("projectionPlane", "None") match {
      case "YZ_PLANE" => YZ_PLANE
      case "XZ_PLANE" => XZ_PLANE
      case "XY_PLANE" => XY_PLANE
      case _ => throw new IllegalArgumentException("No projection plane specified")
      //case s:String => System.err.println("Unknown projection: " +  s ); None
    }
    val useMultiThreading = options.getOrElse("useMultiThreading", "None").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("Unrecognizable useMultiThreading property value: " +  s ); false
    }
    val simplifyLimit:Float = options.getOrElse("simplifyLimit", "0.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("MedianAxisProcessor: unrecognizable simplifyLimit property value: " +  s ); 0f
    }
    val zEpsilon:Float = options.getOrElse("zEpsilon", "1.1") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("MedianAxisProcessor: unrecognizable zEpsilon property value: " +  s ); 1.1f
    }
    val dotProductLimit:Float = options.getOrElse("dotProductLimit", "0.3") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("MedianAxisProcessor: unrecognizable dotProductLimit property value: " +  s ); .3f
    }
    val calculationResolution:Float = options.getOrElse("calculationResolution", MedianAxisOperation.DEFAULT_CALCULATION_RESOLUTION ) match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("MedianAxisProcessor: unrecognizable calculationResolution property value: " +  s ); MedianAxisOperation.DEFAULT_CALCULATION_RESOLUTION.toFloat
    }
    
    if ( calculationResolution < 100) {
      throw new IllegalArgumentException("  must be positive and at least larger than 100")
    }
    
    val rings2D = Rings2DConverter(inModel, projectionPlane, applyWorldTransform=true)
   
    val worldTransformation = if (inModel.hasWorldOrientation()) {
      Option(Matrix4x4Converter(inModel.getWorldOrientation()))
    } else {
      None
    }
    (inModel, rings2D, worldTransformation, zEpsilon, dotProductLimit, calculationResolution, simplifyLimit, objectName, projectionPlane, useMultiThreading)
  }
  
  def computeMedianAxis(majni:MedianAxisJni,rings2D:Rings2DConverter, zEpsilon:Float, dotProductLimit:Float, calculationResolution:Float, simplifyLimit:Float, objectName:String, useMultiThreading:Boolean):Mesh3DConverter = {
    
    var ringSeq = Time.time { majni.loadRings2D(rings2D.mesh2d, simplifyLimit, objectName) }
    ringSeq = Ring2D.sortOutInternals(ringSeq)
    
    println("Starting interiorVoronoiEdges: objectName=" + objectName+ " zEpsilon=" +zEpsilon+ " dotProductLimit=" +dotProductLimit + " calculationResolution="+ calculationResolution + " simplifyLimit="+ simplifyLimit + " useMultiThreading=" + useMultiThreading);
    val internalEdges =
    if (useMultiThreading) 
      Time.time { ringSeq.par.map(ring => Mesh3DConverter.removeZDoubles(majni.voronoiInternalEdges(ring, zEpsilon, dotProductLimit, calculationResolution, simplifyLimit))).toArray }
    else
      Time.time { ringSeq.map(ring => Mesh3DConverter.removeZDoubles(majni.voronoiInternalEdges(ring, zEpsilon, dotProductLimit, calculationResolution, simplifyLimit))).toArray }
   
    val rv = new Mesh3DConverter
    // Do .toArray before assembling result into one Mesh3DConverter
    internalEdges.foreach( mesh3d => 
      mesh3d.getFaces.foreach( es => 
        es.sliding(2,1).foreach(e => {
          rv.addEdge(mesh3d.getVertices(e(0)), mesh3d.getVertices(e(1)))
        })
      )
    )
    rv
  }
    
  def processInput(inMessage:Message) = {
    val majni = MedianAxisJni()
    val returnMessageBuilder = try {
      val (inModel, rings2D, worldTransformation, zEpsilon, dotProductLimit, calculationResolution, simplifyLimit, objectName, projectionPlane, useMultiThreading) = 
        manageInput(inMessage)   
      val result = computeMedianAxis(majni, rings2D, zEpsilon, dotProductLimit, calculationResolution, simplifyLimit, objectName, useMultiThreading)
      println("MedianAxisProcessor found " + result.getFaces.size + " sets of edges")
      val returnPbOutputModel = result.toPBModel(worldTransformation, Option(projectionPlane))
      returnPbOutputModel.setName(inModel.getName + " median axis output")   
      
      val returnMessageBuilder = Message.newBuilder()
      returnMessageBuilder.addModels(returnPbOutputModel)
      val returnPbInputModel = rings2D.toPBModel(true, worldTransformation)
      returnPbInputModel.setName(inModel.getName + " median axis input")
      returnMessageBuilder.addModels(returnPbInputModel)
    } finally {
      majni.destructor
    }
    returnMessageBuilder
  }  
}

object MedianAxisOperation {
  // this is how large the integer voronoi calculation range will be (in c++)
  val DEFAULT_CALCULATION_RESOLUTION = (math.sqrt(Int.MaxValue).toInt * -2).toString
}