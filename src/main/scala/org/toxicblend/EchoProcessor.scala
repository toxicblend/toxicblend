package org.toxicblend

import org.toxicblend.protobuf.ToxicBlendProtos.Message

class EchoProcessor extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val outMessage = Message.newBuilder()
    outMessage.setCommand("ECHO")
    outMessage
  }
}