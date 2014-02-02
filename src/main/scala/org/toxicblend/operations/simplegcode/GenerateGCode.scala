package org.toxicblend.operations.simplegcode

import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Matrix4f
import toxi.geom.AABB
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
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

object GenerateGCode {

  //val gCodeProperties = Map("SizeX"->300f, "SafeZ"->3f, "SpindleSpeed"->1000f, "G0Feedrate" -> 1000f,"G1PlungeFeedrate" -> 100f,"G1Feedrate"->500f) // "DebugGCode"->1f,
  val gCodeProperties = Map("simplifyLimit"->0.05f, "StepDown"->1f, "SizeZ"->5f, "SafeZ"->2f, "SpindleSpeed"->1000f, "G0Feedrate" -> 1000f,"G1PlungeFeedrate" -> 100f,"G1Feedrate"->500f, "DebugGCode"->1f)
  //val gCodeProperties = Map("SafeZ"->5f, "SpindleSpeed"->1000f, "G0Feedrate" -> 1000f,"G1PlungeFeedrate" -> 100f,"G1Feedrate"->500f)

  def gHeader():String = {
    	"G0 Z%s (goto safe z)\n".format(gCodeProperties.get("SafeZ").get.toString)+
    	"M3 S%s (start splindle)\n".format(gCodeProperties.get("SpindleSpeed").get.toInt.toString) +
    	"G4 P3  (dwell 3 seconds)\n" +
    	"G0 F%s (set rapid feedrate)\n".format(gCodeProperties.get("G0Feedrate").get.toInt.toString) + 
    	"G1 F%s (set normal feedrate)\n".format(gCodeProperties.get("G1Feedrate").get.toInt.toString) +
    	"G64 P0.02 Q0.02\n"
  }
    
  def gFooter():String = {
  	var rv:String = "G0 Z%s\n".format(gCodeProperties.get("SafeZ").get.toString)  
  	if (gCodeProperties.get("M101")!=None) rv += "M101\n"
  	rv += "M2\n"
  	rv
  }
    
  def generateGcode(edges:IndexedSeq[Vec2DZ], bb:AABB, gcodeProperties:Map[String,Float]):Array[GCode]= {
    
    val debugGCode = gcodeProperties.get("DebugGCode")!=None 
    val transform:Matrix4f = {
      val scale = {
        val extent = bb.getExtent
	      if (gcodeProperties.get("SizeX")!=None){
	        val sizeX = gcodeProperties.get("SizeX").get
	        val scaleX = sizeX / extent.x
	        new Vec3D(scaleX,scaleX,scaleX)
	      } else if (gcodeProperties.get("SizeY")!=None){
	        val sizeY = gcodeProperties.get("SizeY").get
	        val scaleY = sizeY / extent.y 
	        new Vec3D(scaleY,scaleY,scaleY)
	      } else if (gcodeProperties.get("SizeZ")!=None){
	        val sizeZ = gcodeProperties.get("SizeZ").get
	        val scaleZ = sizeZ / extent.z 
	        new Vec3D(scaleZ,scaleZ,scaleZ)
	      } else {
	        new Vec3D(1f, 1f, 1f)
	      }
	    }
      val min = bb.getMin
	    val offset = new Vec3D(min.x*scale.x,min.y*scale.y,min.z*scale.z)
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
        println("This segment was alreay visited, maybe refresh your map once in a while eh? oid=%d".format(point.objIndex))
      }
      // filter out already visited edges, and sort by angle between previous edge and the next. Straight line wins
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
      if (p == null) point else p
    }
    
    def indexListToCoords(edges:ArrayBuffer[(Int,Int)]):ArrayBuffer[(Float,Float,Float)] = {
      edges.map(e => {
      	val p1 = map(e._1)
		    (p1.getX,p1.getY,p1.getZ)		
		  }) += ({ val p2 = map(edges.last._2); (p2.getX, p2.getY, p2.getZ)}) 
		  	  //  ).map(x => new Vec3D(x._1, x._2, x._3))
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
			val gcodePointArray = walkpath.par.map(s => indexListToCoords(s)).map(x => x.map( y=> new Vec3D(transform.transformOne(new Vec3D(y._1, y._2, y._3)) )))
			gcodePointArray.map(g => new GCode(g.toArray)).toArray
		}
       
    initiateWalkEdge(edges(0)) 
  }  
      
   def saveGCode(filename: String, header:()=>String, inPut:Seq[String], footer:()=>String) = {    
    val writer = new PrintWriter(new File(filename))
    try {
      writer.write(header())
      inPut.foreach(l2=>writer.write(l2))
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
  
  def mesh3d2GCode(mesh:Mesh3DConverter):IndexedSeq[GCode] = {
    val allGCodes = {
      val scaleMToMM = 1000f
      //val simplifyLimit = gCodeProperties.get("simplifyLimit").get  
      val aabb = mesh.getBounds.scaleSelf(scaleMToMM).asInstanceOf[AABB]
      println("Bounding Box: %s".format(aabb.toString))
      val segments = mesh.getEdgesAsVec2DZ(scaleMToMM)
      generateGcode(segments, aabb, gCodeProperties)//.simplify(simplifyLimit
    }
    allGCodes.foreach(g => println(g))
    val totalGCodes = new ArrayBuffer[GCode]
    val stepDown = gCodeProperties.get("StepDown").get  
    var depth = mesh.getBounds.getMin.z 
    while (depth < 0) {
      val filteredGCodes = heightFilter(allGCodes, depth).toArray
      totalGCodes ++= sortByStartPoint(filteredGCodes)
      depth += stepDown
      if (depth > 0f) {
        depth = 0f
      }
    }
    totalGCodes
  }
  
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
  }
}
