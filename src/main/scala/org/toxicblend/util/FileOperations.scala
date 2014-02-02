package org.toxicblend.util

import java.nio.file.{Paths, Files, StandardOpenOption}
import java.nio.charset.{StandardCharsets}
import scala.collection.JavaConverters._
import java.io.Serializable
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.IOException
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.File

object FileOperations {
  
  def write(filePath:String, content:String) = {
    Files.write(Paths.get(filePath), content.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }
  
  def read(filePath:String):String = {
    Files.readAllLines(Paths.get(filePath), StandardCharsets.UTF_8).asScala.mkString
  }
   
  def writeSerializable(filePath:String, content:Serializable) = {  
     val fileOut = new FileOutputStream(filePath)
     try{ 
       val out = new ObjectOutputStream(fileOut)
       try out.writeObject(content) finally out.close()
     } finally fileOut.close() 
  }
  
  def readSerializable(filePath:String):Serializable = {
    val fileIn = new FileInputStream(filePath)
    try {
      val in = new ObjectInputStream(fileIn)
      try in.readObject().asInstanceOf[Serializable] finally in.close()
    } finally fileIn.close()
  }
  
  def absolutePath(filePath:String) = {
   val file = new File(filePath)
   file.getAbsolutePath()
  }
}
