package org.toxicblend.operations.simplegcodegenerator

import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import scala.collection.mutable.ListBuffer
import scala.collection.Map
import scala.collection.mutable.ArrayBuffer
import org.toxicblend.geometry.IntersectionVec3DImplicit._
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.text.ParsePosition

/**
 * A container for the 'last' gcode state written. By keeping this information we can minimize the number of parameters in gcode.
 * The container should be considered an opaque passed on from one gCode* method to the next
 */
protected class GCodeState(val cmd:String, val pos:ReadonlyVec3D, val g0Feed:Float, val g1Feed:Float){}

/**
 * Prints to gcode as text
 * 
 * TODO: don't print X,Y & Z coordinates if they didn't change from the previous line
 * 
 */
class GCode(val gcodePoints:IndexedSeq[Vec3D]) {
  
  val MAGIC_DEPTH_LIMIT = 0.2f
  def startPoint=gcodePoints(0)
  def endPoint=gcodePoints(gcodePoints.size-1)
  def this(input:Array[Float]) = this(input.sliding(3,3).map(x => new Vec3D(x(0), x(1), x(2)) ).toArray)
  
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
          if (segment.size >0 ) {
            rv += new GCode(segment.clone)
            segment.clear
          }
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
          if (segment.size >0 ) {
	          rv += new GCode(segment.clone)
	          segment.clear
          }
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
      rv += new GCode(segment.clone)
      segment.clear
    }
    println("Found %d segments at depth %f".format(rv.size, atDepth))
    println(rv.mkString(","))
    rv.foreach(g => assert(g.gcodePoints.size > 0) )
    //if (rv.size > 0) println(rv(0).gcodePoints.mkString(","))
    rv
  }
  
  /**
   * generate G1 gcode text
   */
  def gCodePlunge(lastState:Option[GCodeState], zPos:Float, gcodeProperties:GCodeSettings):(Option[GCodeState],String) = {
    val c = GCode.floatToString _
    if (lastState.isDefined) {
      val ls = lastState.get
      var rs = ""
      if (ls.pos.z==zPos && ls.g1Feed==gcodeProperties.g1Feedrate){
        // no need to do anything, we are already at the correct position
        (lastState,rs)
      } else {  
        if (ls.cmd == "G1") rs += "\t" else rs += "G1"
        if (zPos <= 0f){
          // plunge in two steps, first to Z=0 with normal speed g1Feedrate. Then to destination Z with g1PlungeFeedrate
          rs += " Z0"
          if (ls.g1Feed != gcodeProperties.g1Feedrate) rs += " F%s".format(c(gcodeProperties.g1Feedrate))
          if (zPos < 0f) rs += "\n\tZ%s F%s".format(c(zPos), c(gcodeProperties.g1PlungeFeedrate))
          (Option(new GCodeState("G1", new Vec3D(ls.pos.x,ls.pos.y,zPos),ls.g0Feed, gcodeProperties.g1PlungeFeedrate)), rs)
        } else {
          if (rs.trim == "G1") {
            rs = "G1 Z%s F%s".format(c(zPos), c(gcodeProperties.g1Feedrate))
          } else {
            rs += "\n\tZ%s F%s".format(c(zPos), c(gcodeProperties.g1Feedrate))
          }
          (Option(new GCodeState("G1", new Vec3D(ls.pos.x,ls.pos.y,zPos),ls.g0Feed, gcodeProperties.g1Feedrate)), rs)
        }
      }
    } else {
      // we should really never end up here in this state
      assert(false)
      (None, "") // g1Feedrate is unknown, by setting it to 0 it will be updated
    }
  }
  
  /**
   * generate G0 gcode text
   */
  def gCodeFastXY(lastState:Option[GCodeState], p:ReadonlyVec3D, gcodeProperties:GCodeSettings):(Option[GCodeState],String) = {
    val c = GCode.floatToString _
    if (lastState.isDefined) {
      val ls = lastState.get
      var rs = ""
      if (ls.cmd == "G0") rs += "\t" else rs += "G0"
      if (ls.pos.x != p.x) rs += " X%s".format(c(p.x))  
      if (ls.pos.y != p.y) rs += " Y%s".format(c(p.y)) 
      if (ls.g0Feed != gcodeProperties.g0Feedrate) rs += " F%s".format(gcodeProperties.g0FeedrateAsString)
      (Option(new GCodeState("G0", new Vec3D(p.x,p.y,ls.pos.z),gcodeProperties.g0Feedrate, ls.g1Feed)), rs.replace("\t ", "\t"))
    } else {
      // we don't know last (complete) position, so we can't return a state
      (None,"G0 X%s Y%s F%s".format(c(p.x), c(p.y), gcodeProperties.g0FeedrateAsString))
    }
  }
  
  /**
   * generate G0 gcode text
   */
  def gCodeFastSafeZ(lastState:Option[GCodeState], gcodeProperties:GCodeSettings):(Option[GCodeState],String) = {
    if (lastState.isDefined) {
      val ls = lastState.get
      var rs = ""
      if (ls.cmd == "G0") rs += "\t" else rs += "G0"
      if (ls.pos.z != gcodeProperties.safeZ) rs += " Z%s".format(gcodeProperties.safeZAsString)
      if (ls.g0Feed != gcodeProperties.g0Feedrate) 
        rs += " F%s".format(gcodeProperties.g0FeedrateAsString)
      if (rs.trim == "G0") {
        // pointless to just print G0 and nothing more
        (lastState,"")
      } else {
        (Option(new GCodeState("G0", new Vec3D(ls.pos.x,ls.pos.y,gcodeProperties.safeZ), gcodeProperties.g0Feedrate, ls.g1Feed)),  rs.replace("\t ", "\t"))
      }
    } else {
      // we don't know last position, so we can't return a state
  	  (None,"G0 Z%s".format(gcodeProperties.safeZAsString))
    }
  }
  
  /**
   * generate G1 gcode text
   */
  def gCodeSlow(lastState:Option[GCodeState], p:ReadonlyVec3D, gcodeProperties:GCodeSettings):(Option[GCodeState],String) = {
    val c = GCode.floatToString _
    if (lastState.isDefined) {
      val ls = lastState.get
      var rs = ""
      if (ls.cmd == "G1") rs += "\t" else rs += "G1"
      if (ls.pos.x != p.x) rs += " X%s".format(c(p.x))  
      if (ls.pos.y != p.y) rs += " Y%s".format(c(p.y))
      if (ls.pos.z != p.z) rs += " Z%s".format(c(p.z))
      if (ls.g1Feed != gcodeProperties.g1Feedrate) rs += " F%s".format(gcodeProperties.g1FeedrateAsString)
      if (rs.trim == "G1") {
        // pointless to just give a "G1" command and nothing more
        (lastState,"")
      } else {
        (Option(new GCodeState("G1", p, ls.g0Feed, gcodeProperties.g1Feedrate)), rs.replace("\t ", "\t"))
      }
    } else {
      (Option(new GCodeState("G1", p, 0, gcodeProperties.g1Feedrate)), 
      "G1 X%s Y%s Z%s F%s".format(c(p.x), c(p.y), c(p.z), c(gcodeProperties.g1Feedrate) ))
    }
  }
  
  /**
   * returns the comparable distance between two points in the XY plane
   */
  @inline protected def xyDistance(p1:ReadonlyVec3D, p2:ReadonlyVec3D):Float = {
    assert(p1!=null) 
    assert(p2!=null)

    if (p1 != null && p2 !=null) {  // TODO: remove these tests
      val dx = p1.x - p2.x
      val dy = p1.y - p2.y
      dx * dx + dy * dy
    } else {
      assert(false) // make sure we never end up here
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
  
  def generateText(lastState:Option[GCodeState], gcodeProperties:GCodeSettings) : (Option[GCodeState], String) = {
	  val rv = new ListBuffer[String]
	  //rv += "\n(generated from :" + this.toString + " )"
	  //rv += gcodePoints.mkString("(",")\n(", ")\n")
	  
	  var state:Option[GCodeState] = if (lastState.isDefined) {
      val ls = lastState.get
	    if (ls.cmd == "G1" && ls.pos.x==startPoint.x && ls.pos.y==startPoint.y && ls.pos.z==startPoint.z && ls.g1Feed==gcodeProperties.g1Feedrate) {
	      // we are already at startPoint, no need to G0 here
	      lastState
	    } else {
	      val (s1,t1) = gCodeFastSafeZ(lastState, gcodeProperties); rv += t1
        val (s2,t2) = gCodeFastXY(s1, startPoint, gcodeProperties); rv += t2
        val (s3,t3) = gCodePlunge(s2, startPoint.z, gcodeProperties); rv += t3
        s3 
	    }
	  } else { 
  	  val (_,t1) = gCodeFastSafeZ(None, gcodeProperties)
  	  if (t1.trim != "G0") rv += t1
  	  val (_,t2) = gCodeFastXY(None, startPoint, gcodeProperties); rv += t2
  	  if (t2.trim != "G0") rv += t2
  	  val s2 = Option(new GCodeState("G0", startPoint, -1, -1))
      val (s3,t3) = gCodePlunge(s2, startPoint.z, gcodeProperties); rv += t3
      s3
  	}
    
	  gcodePoints.tail.foreach(point => {
	     val (s4,t4) = gCodeSlow(state, point, gcodeProperties); rv += t4
	     state = s4
	  })
	  
    //println(rv.mkString("","\n","\n"))
    //println("generated from :" + gcodePoints.mkString(","))
    (state, rv.mkString("","\n","\n"))
  }

  /*override*/ def toStringOldVersion():String = {
    val avgZ = if (gcodePoints.size>0) gcodePoints.foldLeft(0f)((r,c)=>r+c.z)/gcodePoints.size else 0f
    "(StartPoint:%s endPoint:%s gcode.len:%d avgz:%f)".format(startPoint.toString, endPoint.toString, gcodePoints.size, avgZ) 
  }
  
  override def toString():String = {
    def c(aFloat:Float) = "%.1f".format(aFloat).replace(",",".")
    gcodePoints.map(v => "(%s, %s, %s)".format(c(v.x),c(v.y),c(v.z)) ).mkString(",")   
  }
}

object GCode {
    
  /**
   * Locale independent way of converting floats to string. No more problem with "," instead of "." as decimal etc.etc. 
   */
  @inline 
  def floatToString(aFloat:Float):String = {
    decimalFormat.format(aFloat)  
  }
  
  /**
   * Locale independent way of converting strings to float. No more problem with "," instead of "." as decimal etc.etc. 
   */
  @inline 
  def stringToFloat(floatAsString:String):Float = {
    val pos = new ParsePosition(0)
    decimalFormat.parse(floatAsString, pos).floatValue()  // TODO: totally ignoring the pos, fix it
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
