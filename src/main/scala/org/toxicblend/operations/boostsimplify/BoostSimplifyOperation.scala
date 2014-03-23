package org.toxicblend.operations.boostsimplify

import org.toxicblend.ToxicblendException
import org.toxicblend.UnitSystem
import org.toxicblend.util.Time
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.util.Regex
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import toxi.geom.Vec3D
import toxi.geom.LineStrip3D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class BoostSimplifyOperation extends CommandProcessorTrait {
  protected val traceMsg = "BoostSimplifyOperation"
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    
    val useMultiThreading = getMultiThreadingProperty(options,traceMsg)
    val unitScale = getUnitScaleProperty(options,"BoostSimplifyOperation")
    val unitSystem = getUnitSystem(options,traceMsg)
    val simplifyLimit:Float = getFloatProperty(options,"simplifyLimit", 0.1f, traceMsg) / 1000f  // convert from meter to mm
        
    val inverseMatrixes = new ArrayBuffer[Option[Matrix4x4Converter]]
    
    // Convert model vertices to world coordinates so that the simplify scaling makes sense
    val models = inMessage.getModelsList.map(inModel => {
      (Mesh3DConverter(inModel,true), // Unit is now [meter]
      getWorldModel(inModel))
    })
    
    // Perform the simplify operation
    val result = Time.time("Boost Simplify calculation time: ", models.map(model =>{      
      val segments = model._1.findContinuousLineSegments
      val newMesh = new Mesh3DConverter(model._1.name + " boost simplify"); 
      segments._1.foreach(ngon => newMesh.addFace(ngon))
      segments._2.foreach(segment =>  {
        if (segment.size>2) {
          val simplifiedSegment = simplify3D(segment, simplifyLimit)
          newMesh.addEdges(simplifiedSegment)
        } else {
          newMesh.addEdges(segment)
        }
      })
      (newMesh,model._2)
    }))
    
    Time.time("Building resulting pBModel: ",{
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
