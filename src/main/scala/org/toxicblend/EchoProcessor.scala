package org.toxicblend

import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter

class EchoProcessor extends CommandProcessorTrait {

  def processInput(inMessage: Message, options: OptionConverter) = {
    val outMessage = Message.newBuilder()
    outMessage.setCommand("ECHO")
    outMessage
  }
}