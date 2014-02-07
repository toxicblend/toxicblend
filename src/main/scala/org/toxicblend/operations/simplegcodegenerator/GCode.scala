package org.toxicblend.operations.simplegcodegenerator
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import org.toxicblend.geometry.IntersectionVec3DImplicit._
import scala.collection.TraversableOnce.flattenTraversableOnce

object GCode {
  
  def dddsimplify(gcode:Iterator[(Float,Float,Float)],limit:Float):Iterator[(Float,Float,Float)] = {   
    //val output = simplify3D(gcode.map(x => List(x._1, x._2, x._3) ).flatten, limit);
    //output.sliding(3,3).map(x => (x(0), x(1), x(2))).toArray
    val alist = gcode.map(x => List(x._1, x._2, x._3) ).flatten
    alist.sliding(3,3).map(x => (x(0), x(1), x(2)))
  }
}

class GCode(val gcodePoints:Array[Vec3D]) {
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
    var state:() => Unit = null
    val segment = new ArrayBuffer[Vec3D]    
    val rv = new ArrayBuffer[GCode]
    var i = 0

    def stateSearching() {
     //println("stateSearching at z = %f atDepth = %f i=%d".format(gcodePoints(i).z, atDepth, i))

      if ( 0==i ) {
        if ( gcodePoints(i).z - MAGIC_DEPTH_LIMIT <=  atDepth){
          //println("stateSearching at z = %f atDepth = %f i=%d".format(gcodePoints(i).z, atDepth, i))
          val p = gcodePoints(i)
          segment += new Vec3D(p.x, p.y, p.z-atDepth)
          state = stateFound
        }
      } else {
        if (gcodePoints(i).intersectsXYPlane(gcodePoints(i-1), atDepth+MAGIC_DEPTH_LIMIT)){
          //println("stateSearching at z = %f atDepth = %f i=%d".format(gcodePoints(i).z, atDepth, i))
          val p = gcodePoints(i).intersectionPoint(gcodePoints(i-1),atDepth+MAGIC_DEPTH_LIMIT)
          segment += new Vec3D(p.x, p.y, p.z-atDepth)
          state = stateFound
        }
      }
    }
    
    def stateFound() {
      assert(i!=0)
      //println("stateFound at z = %f atDepth = %f i = %d".format(gcodePoints(i).z, atDepth, i))
      if (0<i){
        //println("gcodePoints(i-1).z - MAGIC_DEPTH_LIMIT = %f  i=%d".format(gcodePoints(i-1).z - MAGIC_DEPTH_LIMIT, i))
        assert(gcodePoints(i-1).z - MAGIC_DEPTH_LIMIT <= atDepth )
      }
      if ( gcodePoints(i).z - MAGIC_DEPTH_LIMIT <= atDepth ) {
        val p = gcodePoints(i) 
        segment += new Vec3D(p.x, p.y, p.z-atDepth)
      } else {
	      if (gcodePoints(i).intersectsXYPlane(gcodePoints(i-1), atDepth+MAGIC_DEPTH_LIMIT)){
	        val p = gcodePoints(i).intersectionPoint(gcodePoints(i-1),atDepth+MAGIC_DEPTH_LIMIT)
	        segment += new Vec3D(p.x, p.y, p.z-atDepth)
	        //println("stateFound %d point segment at depth %f".format(segment.size, atDepth))
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

    //println("Searching for segments at depth %f".format(atDepth))
    state = stateSearching
    while ( i < gcodePoints.size ){
      state()
      i+=1  
    }
    if (segment.size >0){
      rv += new GCode(segment.toArray)
      segment.clear
    }
    //println("Found %d segments at depth %f".format(rv.size, atDepth))
    rv
  }
  
  def gCodePlunge(p:ReadonlyVec3D, gcodeProperties:Map[String,Float]):String = {
    val s0 = "G1 X%.5f Y%.5f Z0 F%f".format(p.x, p.y, gcodeProperties.get("G1Feedrate").get).replace(",",".")  // + " ( gCodePlunge " + hashCode.toString + ")"
    val s1 = "G1 X%.5f Y%.5f Z%.5f F%f".format(p.x, p.y, p.z, gcodeProperties.get("G1PlungeFeedrate").get).replace(",",".")
    val s2 = "G1 X%.5f Y%.5f Z%.5f F%f".format(p.x, p.y, p.z, gcodeProperties.get("G1Feedrate").get).replace(",",".") 
    /**if (gcodeProperties.get("DebugGCode")!=None) {
   	  s0 + " (%d)\n".format(p.objIndex) + 
  	  s1 + " (%d)\n".format(p.objIndex) + 
  	  s2 + " (%d)\n".format(p.objIndex)
    } else { */
    s0 + "\n" + s1 + "\n" + s2
    //}
  }
  
  def gCodeFastXY(p:ReadonlyVec3D, gcodeProperties:Map[String,Float]):String = {
    "G0 X%.5f Y%.5f".format(p.x, p.y).replace(",",".") //+ " ( gCodeFastXY " + hashCode.toString + ")"
    /*if (debugGCode) {
    	s1 + " (%d)\n".format(p.objIndex)
    } else {
    
    s1 + "\n" 
    //}*/
  }
  
  def gCodeFastSafeZ(gcodeProperties:Map[String,Float]):String = {
  	"G0 Z%f".format(gcodeProperties.get("SafeZ").get).replace(",",".") //  + " ( gCodeFastSafeZ " + hashCode.toString + ")"
  }
  
  def gCodeSlow(p:ReadonlyVec3D, gcodeProperties:Map[String,Float]):String = {
  	"G1 X%.5f Y%.5f Z%.5f ".format(p.x, p.y, p.z).replace(",",".") //+ " ( gCodeSlow " + hashCode.toString + ")"
  	/**if (debugGCode) {
  	  s1 + " (%s %d)\n".format(traceTxt, p.objIndex)
  	} else { 
    s1 + "\n" 
    //}*/
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
   * returns true if the shortest distance was to the start point
   * false if end point was closer
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
  
  def generateText(gcodeProperties:Map[String,Float]) : String = {
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

  override def toString():String = {
    
    val avgZ = if (gcodePoints.size>0) gcodePoints.foldLeft(0f)((r,c)=>r+c.z)/gcodePoints.size else 0f
    "(StartPoint:%s endPoint:%s gcode.len:%d avgz:%f)".format(startPoint.toString, endPoint.toString, gcodePoints.size, avgZ) 
  }
}

