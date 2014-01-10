package org.toxicblend

import org.toxicblend.protobuf.ToxicBlenderProtos.Message
import org.toxicblend.protobuf.ToxicBlenderProtos.Message.Builder

trait CommandProcessorTrait {
  def processInput(inMessage:Message):Builder
}