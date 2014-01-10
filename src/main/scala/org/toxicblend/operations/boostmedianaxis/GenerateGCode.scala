package org.toxicblend.operations.boostmedianaxis

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import java.io.PrintWriter
import java.io.File
import toxi.geom.ReadonlyVec2D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec2D
import toxi.geom.Vec3D

import org.toxicblend.geometry.Matrix4f


object GenerateGCode {
  /*
  //val gCodeProperties = Map("SizeX"->300f, "SafeZ"->3f, "SpindleSpeed"->1000f, "G0Feedrate" -> 1000f,"G1PlungeFeedrate" -> 100f,"G1Feedrate"->500f) // "DebugGCode"->1f,
  val gCodeProperties = Map("simplifyLimit"->0.05f, "StepDown"->1f, "SizeZ"->4f, "SafeZ"->2f, "SpindleSpeed"->1000f, "G0Feedrate" -> 1000f,"G1PlungeFeedrate" -> 100f,"G1Feedrate"->500f) // "DebugGCode"->1f,
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
    
  def generateGcode(edges:Array[ReadonlyVec3D], bb:BoundingBox, gcodeProperties:Map[String,Float]):Array[GCode]= {
    
    val debugGCode = gcodeProperties.get("DebugGCode")!=None 
    val transform = {
      val scale = {
	      if (gcodeProperties.get("SizeX")!=None){
	        val sizeX = gcodeProperties.get("SizeX").get
	        val scaleX = sizeX / bb.intervalX.length
	        new Vec3D(scaleX,scaleX,scaleX)
	      } else if (gcodeProperties.get("SizeY")!=None){
	        val sizeY = gcodeProperties.get("SizeY").get
	        val scaleY = sizeY / bb.intervalY.length 
	        new Vec3D(scaleY,scaleY,scaleY)
	      } else if (gcodeProperties.get("SizeZ")!=None){
	        val sizeZ = gcodeProperties.get("SizeZ").get
	        val scaleZ = sizeZ / bb.intervalZ.length 
	        new Vec3D(scaleZ,scaleZ,scaleZ)
	      } else {
	        new Vec3D(1f, 1f, 1f)
	      }
	    }
	    val offset = new Vec3D(-bb.intervalX.minRange*scale.x,-bb.intervalY.minRange*scale.y,-bb.intervalZ.maxRange*scale.z)
	    new Matrix4f(offset, scale)
    }
  
    val visited = new HashSet[(Int,Int)] // (objIndex, objIndex)
    val map = new HashMap[Int, ReadonlyVec3D] // (objIndex, Vec2DZ)

    val ret = new ArrayBuffer[ArrayBuffer[(Int,Int)]]  // ArrayBuffer[retSegment]
    var retSegment = new ArrayBuffer[(Int,Int)]
       
    @tailrec
    def walkEdge(point:ReadonlyVec3D, from:ReadonlyVec3D, backlog:List[((ReadonlyVec3D,ReadonlyVec3D))]):ArrayBuffer[ArrayBuffer[(Int,Int)]] ={
      
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
	      //println("Point oid=%d %s".format(point.objIndex, point.edges.map(x => x.objIndex).mkString("(",",",")")))
	      System.out.println() */
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
			val gcodePointArray = walkpath.par.map(s => indexListToCoords(s)).map(x => x.map( y=> new IntersectionVec3D(transform.transformOne3D(new IntersectionVec3D(y._1, y._2, y._3)) )))
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
  def heightFilter(input:Array[GCode], atDepth:Float):ArrayBuffer[GCode] = {
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
  def mySort(input:Array[GCode]):Array[GCode] ={
    var rv = new Array[GCode](input.size)
    val usedGCodes = new HashSet[Int]
    var lastEndPoint:ReadonlyVec3D = new Vec3D()
    for (i <- 0 until input.size){
      var bestSoFar = -1
      var reverseSoFar = false
      var bestDistanceSoFar = Float.PositiveInfinity
	    for (j:Int <- 0 until input.size) {
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
  */
  def main(args: Array[String]): Unit = {
    /*
    val safeZ:Float = 5
    val (objects,bb) = ObjParser.loadObj("data/axel_out.obj")
    println("Found %d object groups".format(objects.length))
    //print(edges)
    println("Bounding Box: %s".format(bb.toString))
    //val fileName="/Volumes/Publikt/test_out.ngc" // "data/
    //val fileName="Data/test_out.ngc" // "data/
    val fileName = "/Users/ead/VirtualBox VMs/share/cnc/axel_out.ngc"
      
    val allGCodes = {
      val simplifyLimit = gCodeProperties.get("simplifyLimit").get  
      objects.map(o => generateGcode(o, bb, gCodeProperties)).flatten.par.map( o => o.simplify(simplifyLimit)).toArray
    }
    
    val totalGCodes = new ListBuffer[GCode]
    val stepDown = gCodeProperties.get("StepDown").get  
    var depth = bb.intervalZ.minRange
    while (depth < 0) {
      val filteredGCodes = heightFilter(allGCodes, depth).toArray
      totalGCodes ++= mySort(filteredGCodes)
      depth += stepDown
      if (depth > 0f) {
        depth = 0f
      }
    }
    saveGCode(fileName, gHeader, totalGCodes.map(g => g.generateText(gCodeProperties)), gFooter)
    
    println("done")
 
    */
  }
}