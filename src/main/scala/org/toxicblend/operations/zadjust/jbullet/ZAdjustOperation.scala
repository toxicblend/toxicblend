package org.toxicblend.operations.zadjust.jbullet

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter

class ZAdjustOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    val modelC = Mesh3DConverter(inMessage.getModels(0))
    val jbc = new JBulletCollision(modelC) 
    jbc.doSomething
    
    val returnMessageBuilder = Message.newBuilder()
    returnMessageBuilder
  }
}