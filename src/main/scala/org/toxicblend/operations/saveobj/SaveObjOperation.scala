package org.toxicblend.operations.saveobj

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.util.FileOperations

class SaveObjOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    val filename:String =  FileOperations.absolutePath(options.getOrElse("filename", "file.toxicblend"))
     
    println("SaveObjOperation Saving to " + filename)
    FileOperations.writeSerializable(filename, inMessage.getModels(0))
    
    val returnMessageBuilder = Message.newBuilder()
    returnMessageBuilder
  }
}