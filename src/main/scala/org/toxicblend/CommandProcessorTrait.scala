package org.toxicblend

import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Message.{Builder => MessageBuilder}
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.util.Regex
import org.toxicblend.typeconverters.Matrix4x4Converter

trait CommandProcessorTrait {
  def processInput(inMessage:Message, options:OptionConverter):MessageBuilder
  
  protected def getWorldOrientation(model:Model) = {
    if (model.hasWorldOrientation) {
      Option(Matrix4x4Converter(model.getWorldOrientation))
    } else {
      None
    }
  }
}