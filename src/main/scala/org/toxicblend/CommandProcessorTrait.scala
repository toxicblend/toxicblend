package org.toxicblend

import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Message.Builder

trait CommandProcessorTrait {
  def processInput(inMessage:Message):Builder
}