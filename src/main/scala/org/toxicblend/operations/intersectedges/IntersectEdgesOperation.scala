package org.toxicblend.operations.intersectedges

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.ToxicblendException
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.MutableList
import toxi.geom.ReadonlyVec3D
import toxi.geom.Line3D
import toxi.geom.Line3D.LineIntersection
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem

import scala.collection.JavaConversions._
import scala.collection.mutable.HashSet

class IntersectEdgesOperation extends CommandProcessorTrait {
  
  /**
   * Naive and simple brute force calculation, but it's good enough for now
   */
  def intersectEdges(modelA:Mesh3DConverter, modelB:Mesh3DConverter):Mesh3DConverter = {
    val rv = new Mesh3DConverter("intersected edges")
    val alreadyDrawnA = new HashSet[(Int,Int)]
    val alreadyDrawnB = new HashSet[(Int,Int)]
    
    val modelAVertices = modelA.getVertices
    val modelBVertices = modelB.getVertices
    modelA.getFaces.foreach(face =>{
      face.sliding(2).foreach(vAseq => {
        val fromA = modelAVertices(vAseq(0))
        val toA = modelAVertices(vAseq(1))
        val keyA = (vAseq(0),vAseq(1))
        val lineA = new Line3D(fromA,toA)
        //println("new lineA=" + lineA)
        modelB.getFaces.foreach(face =>{
          face.sliding(2).foreach(vBseq => {
            val fromB = modelBVertices(vBseq(0))
            val toB = modelBVertices(vBseq(1))
            val lineB = new Line3D(fromB,toB)
               
            val intersection = lineA.closestLineTo(lineB)
            if (intersection.isIntersectionInside) {
              val intersectionLine = intersection.getLine
              //println("lineA=" + lineA)
              //println("lineB=" + lineB)
              //println("int=" + intersectionLine)
              //println
              val keyB = (vBseq(0),vBseq(1))
              alreadyDrawnA.add(keyA)
              alreadyDrawnB.add(keyB)
              rv.addEdges(fromA,intersectionLine.a)
              rv.addEdges(intersectionLine.a,toA)
              //rv.addEdges(intersectionLine.a,intersectionLine.b)
              rv.addEdges(fromB,intersectionLine.b)
              rv.addEdges(intersectionLine.b,toB)
            }
          })
        })
      })
    })
    modelA.getFaces.foreach(face =>{
      face.sliding(2).foreach(vAseq => {
        val fromA = modelAVertices(vAseq(0))
        val toA = modelAVertices(vAseq(1))
        val keyA = (vAseq(0),vAseq(1))
        if (!alreadyDrawnA.contains(keyA)) {
          rv.addEdges(fromA,toA)
        }
      })
    })
    modelB.getFaces.foreach(face =>{
      face.sliding(2).foreach(vBseq => {
        val fromB = modelBVertices(vBseq(0))
        val toB = modelBVertices(vBseq(1))
        val keyB = (vBseq(0),vBseq(1))
        if (!alreadyDrawnB.contains(keyB)) {
          rv.addEdges(fromB,toB)
        }
      })
    })
    rv
  }
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    if (inMessage.getModelsCount() < 2) {
      throw new ToxicblendException("At least two objects must be selected")
    }
    if (inMessage.getModelsCount() != 2) {
      throw new ToxicblendException("This operation required two selected objects")
    }
    
    val models = inMessage.getModelsList().toIndexedSeq.map(i=>Mesh3DConverter(i,true))
    
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("ZAdjustOperation: Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("ZAdjustOperation: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("ZAdjustOperation: Unrecognizable 'unitSystem' property value: " +  s ); None
    } 
    println(options)
    
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = intersectEdges(models(0), models(1))
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}