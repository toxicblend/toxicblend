package org.toxicblend.operations.simplegcodegenerator

import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import scala.collection.mutable.ListBuffer
import scala.collection.Map
import scala.collection.mutable.ArrayBuffer
import org.toxicblend.geometry.IntersectionVec3DImplicit._
import scala.collection.TraversableOnce.flattenTraversableOnce
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols

/**
 * Prints to text
 * I use String.replace(",",".") on the text result, this is because my locale likes to use "," as a decimal point. TODO: fix this in a locale neutral way
 * 
 * TODO: don't print X,Y & Z coordinates if they didn't change from the previous line
 * 
 */
class GCode(val gcodePoints:IndexedSeq[Vec3D]) {
  val MAGIC_DEPTH_LIMIT = 0.2f
  def startPoint=gcodePoints(0)
  def endPoint=gcodePoints(gcodePoints.size-1)
  def this(input:Array[Float]) = this({input.sliding(3,3).map(x => new Vec3D(x(0), x(1), x(2)) ).toArray})
  
  /*
  def this(startPoint:(Float,Float,Float), endPoint:(Float,Float,Float), gcodePoints:Array[(Float,Float,Float)] ) = 
    this(new Vec2DZ(startPoint._1, startPoint._2, startPoint._3),
         new Vec2DZ(endPoint._1, endPoint._2, endPoint._3),
         gcodePoints.map(x => new Vec3D(x._1, x._2, x._3)))
   */
  /**
   * returns a new instance of itself, only reversed in direction
   */
  def reverseDirection():GCode = {
    new GCode(gcodePoints.reverse)
  }
  
  def heightFilter(atDepth:Float):ArrayBuffer[GCode] = { 
    var state: (Vec3D, Vec3D) => Unit = null
    val segment = new ArrayBuffer[Vec3D] 
    val rv = new ArrayBuffer[GCode]
    var i = 0

    /** 
     * Search for a segment that is below or equal to atDepth
     * 
     * fromV   toV     action
     * -----   ------  ------
     * above   above   Do nothing, next state = searching
     * above   below   Store intersection, next state = found
     * below   above   Store fromV and intersection, next state = searching
     * below   below   Store fromV, next state = found
     */
    def stateSearching(fromV:Vec3D, toV:Vec3D) {
     //println("stateSearching at z = %f atDepth = %f i=%d".format(gcodePoints(i).z, atDepth, i))

      //val fromV = gcodePoints(steps(i)(0))
      //val toV = gcodePoints(steps(i)(1))
      if (fromV.z - MAGIC_DEPTH_LIMIT <= atDepth){
        segment += fromV.sub(0f,0f,atDepth)
        println("stateSearching Added segment point: " + segment(segment.size-1)+ " i=" + i+ " depth=" + atDepth)
        if (fromV.intersectsXYPlane(toV, atDepth+MAGIC_DEPTH_LIMIT)){
          segment += fromV.intersectionPoint(toV,atDepth+MAGIC_DEPTH_LIMIT).sub(0f,0f,atDepth)
          println("stateSearching Added segment point: " + segment(segment.size-1)+ " i=" + i+ " depth=" + atDepth)
          rv += new GCode(segment.toArray)
          segment.clear
          state = stateSearching // we found an intersection, but it ended within this state. So we are still searching
        } else {
          assert(toV.z - MAGIC_DEPTH_LIMIT <= atDepth)
          state = stateFound
        } 
      } else if (fromV.intersectsXYPlane(toV, atDepth+MAGIC_DEPTH_LIMIT)){
        //println("stateSearching at z = %f atDepth = %f i=%d".format(gcodePoints(i).z, atDepth, i))
        segment += fromV.intersectionPoint(toV,atDepth+MAGIC_DEPTH_LIMIT).sub(0f,0f,atDepth)
        println("stateSearching Added segment point: " + segment(segment.size-1)+ " i=" + i+ " depth=" + atDepth)
        state = stateFound
      }
    }
    
    /**
     * A segment below or equal to atDepth is found, follow it until it ends or intersects the atDepth plane
     * 
     * fromV   toV     action
     * -----   ------  ------
     * above   above   fail
     * above   below   fail
     * below   above   Store fromV and intersection, next state = searching
     * below   below   Store fromV, next state = found
     */
    def stateFound(fromV:Vec3D, toV:Vec3D) {
      assert( i!=0 )
      //val fromV = gcodePoints(steps(i)(0))
      //val toV = gcodePoints(steps(i)(1))
      
      assert(fromV.z - MAGIC_DEPTH_LIMIT <= atDepth )
      
      if ( toV.z - MAGIC_DEPTH_LIMIT <= atDepth ) {
        segment += fromV.sub(0f,0f,atDepth)
        println("stateFound Added segment point: " + segment(segment.size-1) + " i=" + i+ " depth=" + atDepth)
      } else {
	      if (fromV.intersectsXYPlane(toV, atDepth+MAGIC_DEPTH_LIMIT)){
	        segment += fromV.sub(0f,0f,atDepth)
          println("stateFound Added segment point: " + segment(segment.size-1)+ " i=" + i + " depth=" + atDepth)
	        segment += fromV.intersectionPoint(toV,atDepth+MAGIC_DEPTH_LIMIT).sub(0f,0f,atDepth)
          println("stateFound Added segment point: " + segment(segment.size-1)+ " i=" + i + " depth=" + atDepth)
	        rv += new GCode(segment.toArray)
	        segment.clear
	        state = stateSearching
	      } else {
	        // Should not happen
	        println("Point z=%f and point z=%f should have crossed the limit %f (%f)".format(gcodePoints(i).z,gcodePoints(i-1).z, atDepth, MAGIC_DEPTH_LIMIT))
	        assert(false)
	      } 
      }
    }

    println("Searching for segments at depth %f".format(atDepth))
    state = stateSearching
    i = 0
    (0 until gcodePoints.size).sliding(2).foreach(s => {
      state(gcodePoints(s(0)),gcodePoints(s(1)))
      i+=1
    })
    // One last time, to get the last vertex
    state(gcodePoints(gcodePoints.size-1),gcodePoints(gcodePoints.size-1))

    if (segment.size >0){
      rv += new GCode(segment.toArray)
      segment.clear
    }
    println("Found %d segments at depth %f".format(rv.size, atDepth))
    println(rv.mkString(","))
    if (rv.size > 0) println(rv(0).gcodePoints.mkString(","))
    rv
  }
  
  def gCodePlunge(p:ReadonlyVec3D, gcodeProperties:GCodeSettings):String = {
    val c = GCode.fToString _
    
    val s0 = if (p.z <= 0f){
      "G1 X%s Y%s Z0 F%s".format(c(p.x), c(p.y), c(gcodeProperties.g1Feedrate)) + 
      "\n   Z%s F%s".format(c(p.z), c(gcodeProperties.g1PlungeFeedrate))
    } else {
      "G1 X%s Y%s Z0 F%s".format(c(p.x), c(p.y), c(gcodeProperties.g1Feedrate)) 
    }
    val s1 = "   Z%s F%s".format(c(p.z), c(gcodeProperties.g1Feedrate))
    s0 + "\n" + s1
  }
  
  def gCodeFastXY(p:ReadonlyVec3D, gcodeProperties:GCodeSettings):String = {
    val c = GCode.fToString _
    "G0 X%s Y%s".format(c(p.x), c(p.y)) 
  }
  
  def gCodeFastSafeZ(gcodeProperties:GCodeSettings):String = {
  	"G0 Z%s".format(gcodeProperties.safeZAsString) //  + " ( gCodeFastSafeZ " + hashCode.toString + ")"
  }
  
  def gCodeSlow(p:ReadonlyVec3D, gcodeProperties:GCodeSettings):String = {
    val c = GCode.fToString _
  	"G1 X%s Y%s Z%s ".format(c(p.x), c(p.y), c(p.z) )
  }
  
  protected def xyDistance(p1:ReadonlyVec3D, p2:ReadonlyVec3D):Float = {
    if (p1 != null && p2 !=null) {
      val dx = p1.x - p2.x
      val dy = p1.y - p2.y
      dx * dx + dy * dy
    } else {
       Float.NaN
    }
  }
  
  /**
   * Calculates the (comparable) distance in the XY plane
   * Returns true if the shortest distance was to the start point.
   * Returns false if end point was closer
   */
  def xyDistanceTo(p:ReadonlyVec3D) : (Float,Boolean) = {
    val endDistance = xyDistance(endPoint,p)
    val startDistance = xyDistance(startPoint,p)
    if (endDistance < startDistance) {
      (endDistance, true)
    } else {
      (startDistance,false)
    }
  }
  
  def simplify(limit:Float) : GCode = {
    //val input = new Array[Float](gcodePoints.size*3)
    //(0 until gcodePoints.size).foreach(i => {
    //  input(i*3  ) = gcodePoints(i).x
    //  input(i*3+1) = gcodePoints(i).y
    //  input(i*3+2) = gcodePoints(i).z
    //})
    //val output = simplify3D(gcodePoints, limit)
    //if (gcodePoints.size >= output.size){
    //  new GCode // TODO output)
    //} else {
      this
    //}
  }
  
  def generateText(gcodeProperties:GCodeSettings) : String = {
	  val rv = new ListBuffer[String]
	  //rv += "\n(generated from :" + this.toString + " )"
	  //rv += gcodePoints.mkString("(",")\n(", ")\n")
	  rv += gCodeFastSafeZ(gcodeProperties) 
	  rv += gCodeFastXY(startPoint, gcodeProperties)
	  rv += gCodePlunge(startPoint, gcodeProperties)
	  gcodePoints.tail.foreach(point => rv += gCodeSlow(point, gcodeProperties))
    //println(rv.mkString("","\n","\n"))
    //println("generated from :" + gcodePoints.mkString(","))
    rv.mkString("","\n","\n")
  }

  /*override*/ def toOldString():String = {
    val avgZ = if (gcodePoints.size>0) gcodePoints.foldLeft(0f)((r,c)=>r+c.z)/gcodePoints.size else 0f
    "(StartPoint:%s endPoint:%s gcode.len:%d avgz:%f)".format(startPoint.toString, endPoint.toString, gcodePoints.size, avgZ) 
  }
  
  override def toString():String = {
    gcodePoints.map(v => "(%.1f,%.1f,%.1f)".format(v.x,v.y,v.z).replace(",",".") ).mkString(",")   
  }
}

object GCode {
  
  @inline 
  def fToString(aFloat:Float):String = {
    decimalFormat.format(aFloat)  
  }
  
  val decimalFormat = {
    val df = new DecimalFormat
    df.setGroupingSize(0)
    df.setNegativePrefix("-")
    df.setNegativeSuffix("")
    df.setMultiplier(1)
    df.setMaximumFractionDigits(5)
    val decimalFormatSymbols = new DecimalFormatSymbols
    decimalFormatSymbols.setDecimalSeparator('.')
    df.setDecimalFormatSymbols(decimalFormatSymbols)
    df 
  }
}
