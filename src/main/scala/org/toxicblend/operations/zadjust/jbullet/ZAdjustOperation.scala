package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.ToxicblendException
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.MutableList
import toxi.geom.ReadonlyVec3D
import scala.collection.JavaConversions._

class ZAdjustOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    if (inMessage.getModelsCount() < 2) {
      throw new ToxicblendException("At least two objects must be selected")
    }
    val segments = Mesh3DConverter(inMessage.getModels(0),true).findContinuousLineSegments
    if (segments._1.size > 0) throw new ToxicblendException("First object should only contain edges")
    val models = inMessage.getModelsList().tail.toIndexedSeq.map(i=>Mesh3DConverter(i,true))
    
    val jbc = new JBulletCollision(segments._2, models) 
    val result = new MutableList[IndexedSeq[ReadonlyVec3D]]
    segments._2.foreach(segment => {
      result += jbc.doRayTests(segment)
    })
    
    println("Result:")
    result.foreach( s => {println; s.foreach(r => println(r))} )

    jbc.cleanup
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = Mesh3DConverter(result.toList,"raytests")
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}