package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.ToxicblendException
import scala.collection.mutable.ArrayBuffer
import toxi.geom.ReadonlyVec3D

class ZAdjustOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    
    val segments = Mesh3DConverter(inMessage.getModels(0),true).findContinuousLineSegments
    if (segments._1.size > 0) throw new ToxicblendException("First object should only contain edges")
    val models = (1 until inMessage.getModelsCount()).map(i=>Mesh3DConverter(inMessage.getModels(i),true))
    val jbc = new JBulletCollision(segments._2, models) 
    val result = new ArrayBuffer[Array[ReadonlyVec3D]]
    segments._2.foreach(segment => {
      result += jbc.doRayTests(segment).toArray
    })
    
    jbc.cleanup
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = Mesh3DConverter(result.toArray,"raytests")
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}