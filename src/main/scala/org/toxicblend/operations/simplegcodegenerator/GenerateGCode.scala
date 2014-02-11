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
import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.ToxicblendException

class GenerateGCode(val gCodeProperties:GCodeSettings) {
  
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
    
  def generateGcode(edges:IndexedSeq[IndexedSeq[Vec2DZ]], bb:AABB, gcodeProperties:GCodeSettings):IndexedSeq[GCode]= {
    //val debugGCode = gcodeProperties.get("DebugGCode")!=None 
    val transform:Matrix4f = {
      val scale = {
        val extent = bb.getExtent
	      if (gcodeProperties.sizeX.isDefined ){
	        val sizeX = gcodeProperties.sizeX.get
	        val scaleX = sizeX / extent.x
	        new Vec3D(scaleX,scaleX,scaleX)
	      } else if (gcodeProperties.sizeY.isDefined){
	        val sizeY = gcodeProperties.sizeY.get
	        val scaleY = sizeY / extent.y 
	        new Vec3D(scaleY,scaleY,scaleY)
	      } else if (gcodeProperties.sizeZ.isDefined){
	        val sizeZ = gcodeProperties.sizeZ.get
	        val scaleZ = sizeZ / extent.z 
	        new Vec3D(scaleZ,scaleZ,scaleZ)
	      } else {
	        new Vec3D(1f, 1f, 1f)
	      }
	    }
	    val offset = new Vec3D(0f,0f,0)  // TODO: fix this transform offset, it's just 0 offset now 
	    new Matrix4f(offset, scale)
    }
  
    val visited = new HashSet[(Int,Int)] // (objIndex, objIndex)
    val map = new HashMap[Int, Vec2DZ] // (objIndex, Vec2DZ)

    val ret = new ArrayBuffer[ArrayBuffer[(Int,Int)]]  // ArrayBuffer[retSegment]
    var retSegment = new ArrayBuffer[(Int,Int)]
       
    @tailrec
    def walkEdge(point:Vec2DZ, from:Vec2DZ, backlog:List[((Vec2DZ,Vec2DZ))]):ArrayBuffer[ArrayBuffer[(Int,Int)]] ={
      
      if (!map.contains(point.objIndex)){
        map.put(point.objIndex, point)
      }
      if (!map.contains(from.objIndex)){
        map.put(from.objIndex, from)
      }
      
      if (!visited.contains((point.objIndex,from.objIndex))){
        retSegment +=  new Tuple2(from.objIndex, point.objIndex)
        visited.add((point.objIndex, from.objIndex))
 	      visited.add((from.objIndex, point.objIndex))
	      /*println("oid: %d->%d #connections:%d visited:%s fresh:%s backlog=%s".format(from.objIndex, point.objIndex, point.edges.size, 
	          point.edges.filter(p => visited.contains((point.objIndex,p.objIndex))).map(p=>p.objIndex).mkString(" "),
	          point.edges.filter(p => !visited.contains((point.objIndex,p.objIndex))).map(p=>p.objIndex).mkString(" "), 
	          backlog.map(p=>"%d->%d".format(p._1.objIndex, p._2.objIndex)).mkString(" ")))   
	      println("Point oid=%d %s".format(point.objIndex, point.edges.map(x => x.objIndex).mkString("(",",",")")))
	      System.out.println()
	      */ 
      } else {
        println("This segment was alreay visited: %d->%d".format(point.objIndex, from.objIndex))
      }
      // filter out already visited edges, and sort by angle between previous edge and the next. Straight lines get precedence
      val l:List[Vec2DZ] = point.edges.iterator.filter(p => !visited.contains((point.objIndex, p.objIndex))).toList.sortBy(sx => point.sub(from).angleBetween(sx.sub(point), true))
      //if (l.size > 1)
      //  println(l.map(x=>point.sub(from).angleBetween(x.sub(point), true)).mkString(","))
      l match {
        case Nil => {
          ret += retSegment
          retSegment = new ArrayBuffer[(Int,Int)]
          val bl:List[(Vec2DZ,Vec2DZ)] = {
            val bl:List[(Vec2DZ,Vec2DZ)] = backlog.filter(p => !visited.contains((p._1.objIndex, p._2.objIndex))).toList
            bl.sortBy(p => p._1.distanceToSquared(point))
          }
          bl match  {
            case Nil => ret
            case x :: rest => { 
              walkEdge(x._2, x._1, rest)
          	}
          }
        } 
        case x :: Nil => walkEdge(x, point, backlog)
        case x :: rest => walkEdge(x, point, rest.map(r => (point, r)).toList ::: backlog)
      }
    }
    
    /**
     * Find a point with only one connection
     */
    def findLeaf(point:Vec2DZ): Vec2DZ = {
      val visitedEdges = new HashSet[Vec2DZ]
	    @tailrec def findLeaf(point:Vec2DZ, backlog:List[Vec2DZ] ):Vec2DZ = {
        if (point.edges.size == 1) {
          point
        } else {
          visitedEdges.add(point)
		      point.edges.filter(x => !visitedEdges.contains(x)).toList match {
		        case Nil => {
		          val bl:List[Vec2DZ] = backlog.filter(p => !visitedEdges.contains(p)) 
		          bl match {
		            case Nil => point
		            case x :: rest => findLeaf(x, rest)
		          }
		        }
		        case p :: Nil => findLeaf(p, backlog)
		        case p :: rest => findLeaf(p, rest ::: backlog)
		      }
        }
	    }
      val p = findLeaf(point, Nil)
      if (p == null) 
        point 
      else 
        p
    }
    
    def indexListToCoords(edges:ArrayBuffer[(Int,Int)]):ArrayBuffer[(Float,Float,Float)] = {
      if (edges.size>0){
        edges.map(e => {
        	val p1 = map(e._1)
  		    (p1.getX,p1.getY,p1.getZ)		
  		  }) += ({ val p2 = map(edges.last._2); (p2.getX, p2.getY, p2.getZ)}) 
  		  	  //  ).map(x => new Vec3D(x._1, x._2, x._3))
      } else {
        new ArrayBuffer[(Float,Float,Float)]
      }
    }
    
    /**
     * find a leaf (vertex with only one connection) and setup walkedge from there 
     */
    def initiateWalkEdge(point:Vec2DZ):Array[GCode] = {
		  //val rv = new ListBuffer[String]
		  var startPoint = findLeaf(point)
		  if (startPoint == null) {
		    startPoint = point
		  } 
 
	    val walkpath = walkEdge(startPoint, startPoint, Nil)
			val gcodePointArray = walkpath.par.map(s => indexListToCoords(s)).map(x => x.map( y=> transform.transformOne(new Vec3D(y._1, y._2, y._3)) ))
			gcodePointArray.map(g => new GCode(g.toArray)).toArray
		}
    
    val rv = new ArrayBuffer[GCode]
    edges.foreach(ring => {
      val gcode = initiateWalkEdge(ring(0)) 
      if (gcode.size>0) rv ++= gcode
      else {
        println("could find any gcode from the point"+ ring(0) )
      }
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
  def sortByStartPoint(input:Array[GCode]):Array[GCode] ={
    var rv = new Array[GCode](input.size)
    val usedGCodes = new HashSet[Int]
    var lastEndPoint:ReadonlyVec3D = new Vec3D()
    for (i <- 0 until input.size) yield {
      var bestSoFar = -1
      var reverseSoFar = false
      var bestDistanceSoFar = Float.PositiveInfinity
	    for (j:Int <- 0 until input.size) yield {
	      if( !usedGCodes.contains(j) ){
		      val (distance, reverse) = input(j).xyDistanceTo(lastEndPoint)
		      if (distance < bestDistanceSoFar){
		        reverseSoFar = reverse
		        bestDistanceSoFar = distance
		        bestSoFar = j
		      }
	      }
	    }
      assert(bestSoFar != -1)
      usedGCodes.add(bestSoFar)
      rv(i) = {
        if (reverseSoFar) input(bestSoFar).reverseDirection
        else input(bestSoFar)
      }
      lastEndPoint = rv(i).endPoint
    }
    rv
  }
  
  /**
   * TODO: I simply assume the unit is meter here, - fix it
   */
  def mesh3d2GCode(mesh:Mesh3DConverter):IndexedSeq[GCode] = {
    assert(gCodeProperties.stepDown > 0)
    
    val (allUnadjustedGCodes,aabb) = {
      val scaleMToMM = 1000f
      //val simplifyLimit = gCodeProperties.get("simplifyLimit").get  
      //val aabb = mesh.getBounds.scaleSelf(scaleMToMM).asInstanceOf[AABB]
      val segments = mesh.findContinuousLineSegmentsAsVec2DZ(scaleMToMM)
      if (segments._2.size > 0 && segments._2(0).size > 0) {
        // recalculate the aabb, with the mm conversion and all
        val aabb = new AABB(segments._2(0)(0).asVec3D, 0f)
        segments._2.foreach(segment => segment.foreach(point => aabb.growToContainPoint(point.asVec3D)))
        println("Bounding Box: %s min:%s max:%S".format(aabb.toString, aabb.getMin().toString(), aabb.getMax().toString() ))
        (generateGcode(segments._2, aabb, gCodeProperties).filter(g => g.gcodePoints.size > 0),aabb)
      } else {
        (new ArrayBuffer[GCode], new AABB)
      }
    }
    if (aabb.getMax().z > 0) {
      throw new ToxicblendException("GCode generating edge-meshes must have all vertexed below Z=0")
    }
    println("allUnadjustedGCodes:")
    allUnadjustedGCodes.foreach(g => println(g))
    val layeredGCodes = new ArrayBuffer[GCode]
    var depth = aabb.getMin.z + gCodeProperties.stepDown
    while (depth < 0) {
      val filteredGCodes = heightFilter(allUnadjustedGCodes, depth).toArray
      layeredGCodes ++= sortByStartPoint(filteredGCodes)
      depth += gCodeProperties.stepDown
      if (depth > 0f) {
        depth = 0f
      }
    }
    layeredGCodes ++= allUnadjustedGCodes
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
