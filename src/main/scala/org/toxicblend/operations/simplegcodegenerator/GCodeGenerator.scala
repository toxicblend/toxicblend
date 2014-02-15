package org.toxicblend.operations.simplegcodegenerator

import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Matrix4f
import toxi.geom.AABB
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import org.toxicblend.geometry.Vec2DZ
import org.toxicblend.geometry.IntersectionVec3D
import org.toxicblend.geometry.BoundingBoxDeprecaded
import org.toxicblend.util.FileOperations
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.ToxicblendException

/**
 * class that converts a set of edges into gcode.
 */
class GCodeGenerator(val gCodeProperties:GCodeSettings) {
  
  def gHeader():String = {
  	"G0 Z%s (goto safe z)\n".format(gCodeProperties.safeZAsString) +
  	"M3 S%s (start splindle)\n".format(gCodeProperties.spindleSpeedAsString) +
  	"G4 P3  (dwell 3 seconds)\n" +   // TODO make dwell value a property
  	"G0 F%s (set rapid feedrate)\n".format(gCodeProperties.g0FeedrateAsString) + 
  	"G1 F%s (set normal feedrate)\n".format(gCodeProperties.g1FeedrateAsString) +
  	gCodeProperties.g64Command + "\n"
  }
    
  def gFooter():String = {
  	var rv:String = "G0 Z%s\n".format(gCodeProperties.safeZAsString)  
  	if (gCodeProperties.customEndCommand!="") rv += gCodeProperties.customEndCommand +"\n"
  	rv += "M2\n"
  	rv
  }
    
  def generateGcode(edges:IndexedSeq[IndexedSeq[ReadonlyVec3D]], bb:AABB, gcodeProperties:GCodeSettings):IndexedSeq[GCode]= {
    //val debugGCode = gcodeProperties.get("DebugGCode")!=None 
    val transform:Matrix4f = {
      val scale = {
        val extent = bb.getExtent
	      if (gcodeProperties.sizeX.isDefined ) {
	        val sizeX = gcodeProperties.sizeX.get
	        val scaleX = sizeX / extent.x
	        new Vec3D(scaleX, scaleX, scaleX)
	      } else if (gcodeProperties.sizeY.isDefined) {
	        val sizeY = gcodeProperties.sizeY.get
	        val scaleY = sizeY / extent.y 
	        new Vec3D(scaleY, scaleY, scaleY)
	      } else if (gcodeProperties.sizeZ.isDefined) {
	          val sizeZ = gcodeProperties.sizeZ.get
	          val scaleZ = sizeZ / extent.z 
	          new Vec3D(scaleZ, scaleZ, scaleZ)
	        } else {
	        new Vec3D(1f, 1f, 1f)
	      }
	    }
	    val offset = new Vec3D(0f, 0f, 0)  // TODO: fix this transform offset, it's just 0 offset now 
	    new Matrix4f(offset, scale)
    }
  
    val rv = edges.map( segment => {
      val pGoints = segment.map(point => {  
        val tmp = transform.transformOne(new Vec3D(point.x*1000f, point.y*1000f, point.z*1000f))
        tmp
      })
      new GCode(pGoints)
    }) 
    rv
  }  
      
   def saveGCode(filename:String, header:()=>String, inPut:Seq[String], footer:()=>String) = {    
    val writer = new PrintWriter(new File(filename))
    try {
      writer.write(header())
      inPut.foreach(gcodeLine=>writer.write(gcodeLine))
      writer.write(footer())
    } finally {
      writer.close()
    }
    println("saveGCode:wrote to %s".format(filename))
  }
   
  /**
   * Filter the gcodes so that they only cut at desired depth
   */
  def heightFilter(input:IndexedSeq[GCode], atDepth:Float):ArrayBuffer[GCode] = {
    val rv = new ArrayBuffer[GCode]
    input.foreach(g => rv ++= g.heightFilter(atDepth)) 
    rv
  }
  
  /**
   * Sort the segments so that the previous 'end point' is as close as possible to the next 
   * 'start point' (distance is in XY plane)
   * this could have been easily done with .sortBy or .sortWith but i'm getting a lot of compiler errors
   * "diverging implicit expansion for type scala.math.Ordering...."
   */
  def sortByStartPoint(input:IndexedSeq[GCode]):IndexedSeq[GCode] ={
    var rv = new Array[GCode](input.size)
    val usedGCodes = new HashSet[Int]
    // The next startpoint will be the one closest to origo
    var lastEndPoint:ReadonlyVec3D = new Vec3D(0,0,0) 
    (0 until input.size).foreach( i => {
      var bestSoFar = -1
      var reverseSoFar = false
      var bestDistanceSoFar = Float.PositiveInfinity
	    (0 until input.size).foreach( j => {
	      if( !usedGCodes.contains(j) ){
		      val (distance, reverse) = input(j).xyDistanceTo(lastEndPoint)
		      if (distance < bestDistanceSoFar){
		        reverseSoFar = reverse
		        bestDistanceSoFar = distance
		        bestSoFar = j
		      }
	      }
	    })
	    if (bestSoFar == -1) {
	      println("c'est ne pas une break point")
	    }
      assert(bestSoFar != -1)
      usedGCodes.add(bestSoFar)
      rv(i) = {
        if (reverseSoFar) {
          input(bestSoFar).reverseDirection
        } else {
          input(bestSoFar)
        }
      }
      lastEndPoint = rv(i).endPoint
    })
    rv
  }
  
  /**
   * TODO: I simply assume the unit is meter here, - fix it
   */
  def mesh3d2GCode(mesh:Mesh3DConverter):IndexedSeq[GCode] = {
    assert(gCodeProperties.stepDown > 0)
    
    val (allUnadjustedGCodes, aabb) = {
      val scaleMToMM = 1000f
      //val simplifyLimit = gCodeProperties.get("simplifyLimit").get  
      //val aabb = mesh.getBounds.scaleSelf(scaleMToMM).asInstanceOf[AABB]
      val segments = mesh.findContinuousLineSegments
      if (segments._2.size > 0 && segments._2(0).size > 0) {
          // recalculate the aabb, with the mm conversion and all
          val aabb = new AABB(segments._2(0)(0).scale(1000f), 0f) // meter to mm
          segments._2.foreach(segment => segment.foreach(point => aabb.growToContainPoint(point.scale(1000f)))) // meter to mm
          println("Bounding Box: %s min:%s max:%S".format(aabb.toString, aabb.getMin().toString(), aabb.getMax().toString() ))
          (generateGcode(segments._2, aabb, gCodeProperties).filter(g => g.gcodePoints.size > 0),aabb)
        } else {
        (new ArrayBuffer[GCode], new AABB)
      }
    }
    if (aabb.getMax().z > 0) {
      throw new ToxicblendException("GCode generating edge-meshes must have all vertexes below Z=0")
    }
    //println("allUnadjustedGCodes:")
    //allUnadjustedGCodes.foreach(g => println(g))
    val layeredGCodes = new ArrayBuffer[GCode]
    var depth = aabb.getMin.z + gCodeProperties.stepDown
    while (depth < 0) {
      val filteredGCodes = heightFilter(allUnadjustedGCodes, depth).toArray
      if (filteredGCodes.size > 1) {   
        layeredGCodes ++= sortByStartPoint(filteredGCodes) 
      }
      depth += gCodeProperties.stepDown
      if (depth > 0f) {
        depth = 0f
      }
    }
    layeredGCodes ++= sortByStartPoint(allUnadjustedGCodes)
  }
  /*
  def main(args: Array[String]): Unit = {
    
    val inFilename = "gcode.toxicblend"
    val outFilename = "/Users/ead/VirtualBox VMs/share/cnc/gcode.ngc"
    val safeZ:Float = 5
    val mesh = {
      val testObject = FileOperations.readSerializable(inFilename).asInstanceOf[Model]
      Mesh3DConverter(testObject,true)
    }
    val totalGCodes = mesh3d2GCode(mesh)
    //println("gcodes:")
    //totalGCodes.foreach(g => println(g))
    saveGCode(outFilename, gHeader, totalGCodes.map(g => g.generateText(gCodeProperties)), gFooter)
    println("done")
  }*/
}
