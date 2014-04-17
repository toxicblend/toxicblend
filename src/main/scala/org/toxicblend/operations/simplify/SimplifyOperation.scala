package org.toxicblend.operations.simplify

import org.toxicblend.ToxicblendException
import org.toxicblend.UnitSystem
import org.toxicblend.util.Time.time
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Regex
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.{simplify3D => boostSimplify}
import toxi.geom.ReadonlyVec3D
import toxi.geom.LineStrip3D
import scala.collection.mutable.ArrayBuffer
import org.toxicblend.geometry.RamerDouglasPeuckerAlgorithm.{simplify=>javaSimplify}
import scala.collection.JavaConversions._

/**
 * Ramer-Douglas-Peucker 3d simplify implementation
 * The addon can select either the boost (jni) or a scala implementation.
 * It turns out that the java implementation is faster, so the boost implementation will probably be removed.
 */
class SimplifyOperation extends CommandProcessorTrait {
  protected val traceMsg = "SimplifyOperation"
  
  protected def runSimplify(simplifyLimit:Float, useBoost:Boolean, segment:IndexedSeq[ReadonlyVec3D], newMesh:Mesh3DConverter):Unit = {
    if (segment.size>2) {
      val simplifiedSegment = if (useBoost){
        boostSimplify(segment, simplifyLimit)
      } else {
        javaSimplify(segment, simplifyLimit)
      }
      newMesh.synchronized {
        newMesh.addEdges(simplifiedSegment)
      }
    } else {
      newMesh.synchronized {
        newMesh.addEdges(segment)
      }
    }
  }
  
  def processInput(inMessage:Message, options:OptionConverter) = {
    
    val useMultiThreading = options.getMultiThreadingProperty(traceMsg,true)
    //if (useMultiThreading) System.err.println(traceMsg + ":useMultiThreading=True but it's not implemented yet")
    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitSystem = options.getUnitSystemProperty(traceMsg)
    val simplifyLimit = options.getFloatProperty("simplifyLimit", 0.1f, traceMsg) *unitScale/1000f // convert from meter to mm
    val useBoost = options.getBooleanProperty("useBoost", true, traceMsg)
    val inverseMatrixes = new ArrayBuffer[Option[Matrix4x4Converter]]
    
    // Convert model vertices to world coordinates so that the simplify scaling makes sense
    val models = inMessage.getModelsList.map(inModel => {
      (Mesh3DConverter(inModel,true), // Unit is now [meter]
      getWorldOrientation(inModel))
    })
        
    // Perform the simplify operation
    val result = models.map(model =>{      
      val segments = time("find continuous line segments: ", model._1.findContinuousLineSegments)
      val (newMesh, timeMessage) = if (useBoost) {
        (new Mesh3DConverter(model._1.name + " boost simplify"),"Boost Simplify calculation time: ")
      } else {
        (new Mesh3DConverter(model._1.name + " simplify"), "Simplify calculation time: ")
      }
      segments._1.foreach(ngon => newMesh.addFace(ngon))
      
      time(timeMessage, 
        if (useMultiThreading) 
          segments._2.par.foreach(segment => runSimplify(simplifyLimit, useBoost, segment, newMesh))
        else
          segments._2.foreach(segment => runSimplify(simplifyLimit, useBoost, segment, newMesh))
      )
      (newMesh,model._2)
    })
    
    time("Building resulting pBModel: ",{
      val returnMessageBuilder = Message.newBuilder
      // Convert the coordinate system back again (world orientation matrix.inverse)
      result.foreach(mesh3d => {
        val pbModel = 
        if (mesh3d._2.isDefined) {
          val worldOrientation = mesh3d._2.get
          val worldOrientationI = worldOrientation.matrix.copy; worldOrientationI.invert
          mesh3d._1.transform(worldOrientationI)
          
          val pbModel = mesh3d._1.toPBModel(None, None)
          pbModel.setWorldOrientation(worldOrientation.toPBModel)
          pbModel
        } else {
          mesh3d._1.toPBModel(None, None)
        }
        returnMessageBuilder.addModels(pbModel)
      })
      returnMessageBuilder
    })
  }
}
