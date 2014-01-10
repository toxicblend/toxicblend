package org.toxicblend

import org.toxicblend.protobuf.ToxicBlenderProtos.Message

class EchoProcessor extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val outMessage = Message.newBuilder()
    outMessage.setCommand("ECHO")
    outMessage
  }
}