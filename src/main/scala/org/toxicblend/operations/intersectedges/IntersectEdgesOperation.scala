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
import scala.collection.mutable.HashMap

class IntersectEdgesOperation extends CommandProcessorTrait {
  
  /**
   * Naive and simple brute force calculation, but it's good enough for now
   */
  def intersectEdges(modelA:Mesh3DConverter, modelB:Mesh3DConverter):Mesh3DConverter = {
    val rv = new Mesh3DConverter("intersected edges")
    val alreadyDrawnA = new HashSet[(Int,Int)]
    val alreadyDrawnB = new HashMap[(Int,Int),ArrayBuffer[ReadonlyVec3D]]
    
    val modelAVertices = modelA.getVertices
    val modelBVertices = modelB.getVertices
    val lineAintersections = new ArrayBuffer[ReadonlyVec3D]
    modelA.getFaces.foreach(face =>{
      face.sliding(2).foreach(vAseq => {
        val fromA = modelAVertices(vAseq(0))
        val toA = modelAVertices(vAseq(1))
        val keyA = (vAseq(0),vAseq(1))
        val lineA = new Line3D(fromA,toA)
        lineAintersections.clear
        lineAintersections.add(fromA)
        //println("new lineA=" + lineA)
        modelB.getFaces.foreach(face =>{
          face.sliding(2).foreach(vBseq => {
            val keyB = (vBseq(0),vBseq(1))
            if (true) { //!alreadyDrawnB.contains(keyB)) {
              val fromB = modelBVertices(vBseq(0))
              val toB = modelBVertices(vBseq(1))
              val lineB = new Line3D(fromB,toB)
                 
              val intersection = lineA.closestLineTo(lineB)
              if (intersection.isIntersectionInside && intersection.getLine().getLengthSquared() < 0.0001) {
                val intersectionLine = intersection.getLine
                //println("lineA=" + lineA)
                //println("lineB=" + lineB)
                //println("int=" + intersectionLine)
                //println
                lineAintersections += intersectionLine.a
                //alreadyDrawnA.add(keyA)
                val lineBintersections = if (alreadyDrawnB.contains(keyB)) {
                  alreadyDrawnB(keyB)
                } else {
                  val newIntersectionArray = new ArrayBuffer[ReadonlyVec3D]
                  newIntersectionArray.add(fromB)
                  alreadyDrawnB.put(keyB,newIntersectionArray)
                  newIntersectionArray
                } 
                lineBintersections += intersectionLine.b
                //rv.addEdges(fromA,intersectionLine.a)
                //rv.addEdges(intersectionLine.a,toA)
                //rv.addEdges(intersectionLine.a,intersectionLine.b)
                //rv.addEdges(fromB,intersectionLine.b)
                //rv.addEdges(intersectionLine.b,toB)
                
                // TODO: what if intersection is parallel?
              }
            }
          })
        })
        // add the model A edge intersections in correct order
        if (lineAintersections.size > 1) {
          alreadyDrawnA.add(keyA)
          //println("fromA" + fromA)
          //println("b4 sort: " + lineAintersections)
          val lineAsortedIntersections = lineAintersections.sortBy(intersection => fromA.distanceToSquared(intersection))
          lineAsortedIntersections.add(toA)
          //println("after sort: " + lineAintersections)
          lineAsortedIntersections.sliding(2,1).foreach( aIntersectionPoints => {
            //println("adding edge : " + aIntersectionPoints(0) + " -> " + aIntersectionPoints(1))
            rv.addEdges( aIntersectionPoints(0), aIntersectionPoints(1))
          })
        }
      })
    })
    // add the model B edge intersections in correct order
    alreadyDrawnB.foreach( b => {
      val fromB = modelBVertices(b._1._1)
      val toB = modelBVertices(b._1._2)
      val lineBinterserctions = b._2.sortBy(intersection => fromB.distanceToSquared(intersection))
      //println("fromB" + fromB)
      //println("toB" + toB)
      //println("b4 sort: " + lineBinterserctions)
      lineBinterserctions.add(toB)
      //println("after sort: " + lineBinterserctions)
      lineBinterserctions.sliding(2,1).foreach( bIntersectionPoints => {
        //println("adding edge : " + bIntersectionPoints(0) + " -> " + bIntersectionPoints(1))
        rv.addEdges( bIntersectionPoints(0), bIntersectionPoints(1))
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