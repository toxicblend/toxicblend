package org.toxicblend.operations.intersectedges

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.ToxicblendException
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.MutableList
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.Line3D
import toxi.geom.Matrix4x4
import org.toxicblend.geometry.Matrix4x4Extension
import toxi.geom.Line3D.LineIntersection
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem

import scala.collection.JavaConversions._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.math

trait HasContainsMethod[A] {
  def contains(key: A): Boolean
}

class IntersectEdgesOperation extends CommandProcessorTrait {
    
  /**
   * Naive and simple brute force "algorithm": test each edge in model against all edges in model B.
   */
  def intersectEdges(modelA:Mesh3DConverter, modelB:Mesh3DConverter):Mesh3DConverter = {
    val rv = new Mesh3DConverter("intersected edges")
    val alreadyDrawnA = new HashSet[(Int,Int)] with HasContainsMethod[(Int,Int)]
    val alreadyDrawnB = new HashMap[(Int,Int),ArrayBuffer[ReadonlyVec3D]] with HasContainsMethod[(Int,Int)]
    var intersectingEdges = 0
    var smallestIntersectionDistance = Float.MaxValue
    
    val modelAVertices = modelA.getVertices
    val modelBVertices = modelB.getVertices
    val lineAintersections = new ArrayBuffer[ReadonlyVec3D]
    println("ModelA bounds: " + modelA.getBounds)
    println("ModelB bounds: " + modelB.getBounds)
    modelA.getFaces.foreach(face =>{
      face.sliding(2).foreach(vAseq => {
        val fromA = modelAVertices(vAseq(0))
        val toA = modelAVertices(vAseq(1))
        val keyA = (vAseq(0),vAseq(1))
        val lineA = new Line3D(fromA,toA)
        lineAintersections.clear
        lineAintersections.add(fromA)
        modelB.getFaces.foreach(face =>{
          face.sliding(2).foreach(vBseq => {
            val keyB = (vBseq(0),vBseq(1))
            val fromB = modelBVertices(vBseq(0))
            val toB = modelBVertices(vBseq(1))
            val lineB = new Line3D(fromB,toB)
               
            val intersection = lineA.closestLineTo(lineB)
            if (intersection.isIntersectionInside) {
              val distance = intersection.getLine().getLengthSquared()
              smallestIntersectionDistance = Math.min(smallestIntersectionDistance, distance)
              if ( distance < IntersectEdgesOperation.ε) {
                val intersectionLine = intersection.getLine
                
                // I separate intersectionLine.a and intersectionLine.b here, but they should be (almost) identical
                lineAintersections += intersectionLine.a
                val lineBintersections = if (alreadyDrawnB.contains(keyB)) {
                  alreadyDrawnB(keyB)
                } else {
                  val newIntersectionArray = new ArrayBuffer[ReadonlyVec3D]
                  newIntersectionArray.add(fromB)
                  alreadyDrawnB.put(keyB,newIntersectionArray)
                  newIntersectionArray
                } 
                lineBintersections += intersectionLine.b
                // TODO: what if intersecting lines are parallel?
              }
            }
          })
        })
        // add the model A edge intersections in correct order
        if (lineAintersections.size > 1) {
          alreadyDrawnA.add(keyA)
          intersectingEdges += 1
          val lineAsortedIntersections = lineAintersections.sortBy(intersection => fromA.distanceToSquared(intersection))
          lineAsortedIntersections.add(toA)
          lineAsortedIntersections.sliding(2,1).foreach( aIntersectionPoints => {
            rv.addEdge( aIntersectionPoints(0), aIntersectionPoints(1))
          })
        }
      })
    })
    // add the model B edge intersections in correct order
    alreadyDrawnB.foreach( b => {
      intersectingEdges += 1
      val fromB = modelBVertices(b._1._1)
      val toB = modelBVertices(b._1._2)
      val lineBinterserctions = b._2.sortBy(intersection => fromB.distanceToSquared(intersection))
      lineBinterserctions.add(toB)
      lineBinterserctions.sliding(2,1).foreach( bIntersectionPoints => {
        rv.addEdge( bIntersectionPoints(0), bIntersectionPoints(1))
      })
    })
    
    // add the non intersecting edges just as they were
    def addAllNonIntersectingEdges(model:Mesh3DConverter, set:HasContainsMethod[(Int,Int)], vertices:Seq[ReadonlyVec3D]) = {
      model.getFaces.foreach(face =>{
        face.sliding(2).foreach(vSeq => {
          val fromV = vertices(vSeq(0))
          val toV = vertices(vSeq(1))
          val keyV = (vSeq(0),vSeq(1))
          if (!set.contains(keyV)) {
            rv.addEdge(fromV,toV)
          }
        })
      })
    }
    println("Found " + intersectingEdges + " intersecting edges")
    val strD = if (smallestIntersectionDistance==Float.MaxValue) "Float.MaxValue" else smallestIntersectionDistance.toString
    println("Smallest intersection distance = " + strD) 
    addAllNonIntersectingEdges(modelA, alreadyDrawnA, modelAVertices)
    addAllNonIntersectingEdges(modelB, alreadyDrawnB, modelBVertices)
    
    rv
  }
  
  /**
   * Sometimes the blender models will get much too small for numerical stability in the 
   * Line3D.closestLineTo method. 
   * This methods tries to find a matrix that will center and scale the mesh to a more sane scale
   */
  def getSanityScaling(modelA:Mesh3DConverter, modelB:Mesh3DConverter):Matrix4x4 = {
    val boundsA = modelA.getBounds
    val boundsB = modelB.getBounds
    
    val maxExtent:ReadonlyVec3D = if (boundsA.getExtent.magSquared > boundsB.getExtent.magSquared) boundsA.getExtent else  boundsB.getExtent
    val maxOffset:ReadonlyVec3D = if (boundsA.magSquared > boundsB.magSquared) boundsA else boundsB
    val targetExtent = 1000f  // the new extent, is it too large?
    val scale = targetExtent/Math.max(Math.max(maxExtent.x,maxExtent.y),maxExtent.y)
    //println("maxOffset=" + maxOffset)
    //println("maxExtent=" + maxExtent)
    //println("scale=" + scale)
    val matrix = new Matrix4x4Extension(maxOffset.scale(-1f), new Vec3D(scale,scale,scale)) 
    //println("sanity matrix=" + matrix)
    matrix
  }
  
  
  def processInput(inMessage:Message) = {
    val options = OptionConverter(inMessage)
    if (inMessage.getModelsCount != 2) {
      throw new ToxicblendException("This operation requires two selected objects")
    }
    
    val models = inMessage.getModelsList().toIndexedSeq.map(i=>Mesh3DConverter(i,true))
    
    val useMultiThreading = options.getOrElse("useMultiThreading", "FALSE").toUpperCase() match {
      case "TRUE" => true
      case "FALSE" => false
      case s:String => System.err.println("IntersectEdgesOperation: Unrecognizable 'useMultiThreading' property value: " +  s ); false
    }
    val unitScale:Float = options.getOrElse("unitScale", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("IntersectEdgesOperation: unrecognizable 'unitScale' property value: " +  s ); 1f
    }
    val unitIsMetric = options.getOrElse("unitSystem", "METRIC").toUpperCase() match {
      case "METRIC" => UnitSystem.Metric
      case "NONE" => None
      case "IMPERIAL" => UnitSystem.Imperial
      case s:String => System.err.println("IntersectEdgesOperation: Unrecognizable 'unitSystem' property value: " +  s ); None
    } 
    println(options)
    
    val returnMessageBuilder = Message.newBuilder()
    val matrix = getSanityScaling(models(0), models(1))
    models.foreach( m => m.transform(matrix))
    val returnMeshConverter = intersectEdges(models(0), models(1))
    val intertedM = matrix.invert
    //println("Inverted=" + intertedM)
    returnMeshConverter.transform(intertedM)
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}

object IntersectEdgesOperation {
  val ε:Float = 0.00001f
}