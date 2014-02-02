package org.toxicblend.operation.simplegcode

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.io.Source
import scala.math.max
import scala.math.min
import java.io.PrintWriter
import java.io.File
import scala.math
import toxi.geom.ReadonlyVec2D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.AABB
import org.toxicblend.geometry.BoundingBoxDeprecaded
import org.toxicblend.geometry.BoundingBoxMutableDeprecaded
import org.toxicblend.geometry.Vec2DZ

/**
 * com.badlogic.gdx.graphics.g3d.loaders.wavefront.ObjLoader refuses to import meshes without faces.
 * This class does just that (and only that)
 */
object ObjParser { //extends RingParser {
  
  /*
  def saveEdgesToObj(filename: String, inEdges:Array[InteriorEdges]) = {
    var vertices = new ArrayBuffer[String]
    var faces = new ArrayBuffer[String]
    val totalText = new ArrayBuffer[String]
    var prevVert = -1
    var curVert = 0
    var objectNameIndex = 0
    
    for(edges <- inEdges) {
      prevVert = -1
	    if (edges==null||edges.numberOfVertexes==0){
	      System.out.println("saveObj: empty array");
	    } else {
	      totalText += "o median%d\n".format(objectNameIndex)
	      
	      objectNameIndex += 1 
	      prevVert = -1
	      /*edges.interiorEdges.foreach(el => {  
          prevVert = -1
          el.iterator.sliding(3,3).foreach(ep=>{
            if(ep(2).isNaN){
              println("NaN detected")
            }
            val verticess = "v %f %f %f\n".format(ep(0), ep(1), ep(2)).replace(",",".")
            //print(verticess)
            vertices += verticess
            if (-1==prevVert){
              prevVert = curVert
              curVert += 1
            } else {
              prevVert = curVert
              curVert += 1
              val facesStr = "f %d %d\n".format(prevVert,curVert)
              //print(facess)
              faces += facesStr
            }
           })
         })
         */
	     }
       totalText.appendAll(vertices)
       totalText.appendAll(faces)
       vertices.clear
       faces.clear
    }
    val writer = new PrintWriter(new File(filename))
    try {
      totalText.foreach(l=>writer.write(l))
    } finally {
      writer.close()
    }
    println("Wrote to %s".format(filename))
  }  
  */
  
  protected def edges2Text(totalText:ArrayBuffer[String], vertexNumbering:Int, ringId:Int, inEdges:Array[Vec2DZ]):Int = {
    var vertices = new ArrayBuffer[String]
    var faces = new ArrayBuffer[String]
    
    for(i <- 0 until inEdges.size){
      inEdges(i).objIndex = vertexNumbering+i+1
    }
    val newVertexNumbering = inEdges.size+vertexNumbering
    
    val alreadyProcessed = new HashSet[Vec2DZ]
    totalText += "o ringId%d\n".format(ringId) 
	      
    for(p <- inEdges) {
      if (!alreadyProcessed.contains(p)){
          if (p.getZ < 0f || p.getZ.isNaN){
            println("NaN detected")
          }
          // format "%d".format(float) uses ',' for decimal maker in some locales. toString does not 
          vertices +=  "v %f %f %f\n".format(p.getX, p.getY, -math.sqrt(p.getZ)).replace(",",".")
          for (neigbour <-p.edges){
             if (neigbour.eq(p)){
               println("edges2Text: Strange, i found a vertex having an edge to itself")
             } else {
	             if (!alreadyProcessed.contains(neigbour)){
	               faces += "f %d %d\n".format(p.objIndex, neigbour.objIndex) 
	             }
             }
          }
      }
    } 

    totalText.appendAll(vertices)
    totalText.appendAll(faces)
    newVertexNumbering
  } 
  
  def saveEdgesToObj(filename: String, inGroups:Array[(Int,Array[Vec2DZ])]) = {
    val totalText = new ArrayBuffer[String]
    var vertexNumbering:Int=0
    //var objectNameIndex:Int=1
 	      
    for((ringId,p) <- inGroups) {
      vertexNumbering = edges2Text(totalText,vertexNumbering,ringId, p)
    } 
    
    val writer = new PrintWriter(new File(filename))
    try {
      totalText.foreach(l=>writer.write(l))
    } finally {
      writer.close()
    }
    println("Wrote to %s".format(filename))
  }  
   /*
  protected def saveSubRingsAsObj( ring:Ring2D, inVertNumber:Int, totalText:ListBuffer[String]):Int = {
    val verts = new ListBuffer[String];
    val faces = new ListBuffer[String];
    var vertNumber = inVertNumber
    verts += "o ring%d\n".format(ring.ringId)
    var firstVert = vertNumber
    var prevVertNumber = -1
    for (i <- 0 until ring.verts.length by 2) {
      verts += "v %f %f 0\n".format(ring.verts(i), ring.verts(i+1)).replace(",", ".")
      if (-1 != prevVertNumber){
        faces += "f %d %d\n".format(prevVertNumber, vertNumber)
      }
      prevVertNumber = vertNumber
      vertNumber +=1
    }
    if (-1 != prevVertNumber){
      faces += "f %d %d\n".format(prevVertNumber, firstVert)
    }
    totalText ++= verts
    totalText ++= faces
    for (r <- ring.subRings) {
      vertNumber = saveSubRingsAsObj(r, vertNumber, totalText)
    }
    return vertNumber
  }
 
  def saveRingsAsObj(filename:String, inRings:Array[Ring2D]):Unit = {
    val totalText = new ListBuffer[String];
    var vertNumber = 1
    for (r <- inRings) {
      vertNumber = saveSubRingsAsObj(r, vertNumber, totalText)
    }
    val writer = new PrintWriter(new File(filename))
    try {
      totalText.foreach(l=>writer.write(l))
    } finally {
      writer.close()
    }
    println("Wrote rings to %s".format(filename))
  }
  
  def loadObj(ma:MedianAxisJni, filename: String, simplifyLimit:Float):Array[Ring2D] = {
		val verts: ArrayBuffer[ReadonlyVec3D] = new ArrayBuffer(0);
		val faces: ArrayBuffer[Array[Int]] = new ArrayBuffer(0);
		val vertexNeigbours: HashMap[Int,Array[Int]] = new HashMap();
		val bb = new AABB
		var objectName = "";		
		var state:(String)=>Unit = null
	
    def parseObject(line:String):Unit = {
		  //println(line)
      if (line.startsWith("o ")) {
        objectName = line.substring(2)
        println(objectName)
      } else if (line.startsWith("v")) {
        state = parseVerts
        state(line)
      }
    }
		
		def parseVerts(line:String):Unit = {
 		  //println(line)
      if (line.startsWith("v ")) {
        val v3 = new Vec3D(line.split(" ").drop(1).map( x => x.toFloat ))
        if (v3.z > 0.001f || v3.z < -0.001f) {
          println("your .obj vertex data is probably incorrect. Expecting x,y,z=0 not z=" + v3.z)
        }
        bb.growToContainPoint(v3)        
        verts+=v3
      } else if (line.startsWith("f ")) {
		    state = parseFaces
		    state(line)
		  }
    }
		
		def parseFaces(line:String):Unit = {
 		  //println(line)
      if (line.startsWith("o ")) {
        state = parseObject
		    state(line)
      } else if (line.startsWith("f ")) {
        val fline = line.split(" ").drop(1).map( x => x.toInt-1 )
        for (f2 <- 0 to fline.length-2 ){
          for (f3 <- f2+1 to fline.length-1 ){

            if (fline(f2)< 0 || fline(f2) > verts.size-1){
	      	    println("ERROR: some faces are incorrect: %d ".format(fline(f2)))
	      	  }
            
            if (fline(f3)< 0 || fline(f3) > verts.size-1){
	      	    println("ERROR: some faces are incorrect: %d ".format(fline(f3)))
	      	  }
            
            assert(f2!=f3)
            assert(fline(f2)!=fline(f3))
	          if (vertexNeigbours.contains(fline(f2))){
	            vertexNeigbours.put(fline(f2),vertexNeigbours(fline(f2)) :+ fline(f3))
	          } else {
	            vertexNeigbours.put(fline(f2),Array(fline(f3)))
	          }
            
            if (vertexNeigbours.contains(fline(f3))){
              val oldVal = vertexNeigbours(fline(f3))
              val newVal = oldVal :+ fline(f2)
	            vertexNeigbours.put(fline(f3),newVal)
	          } else {
	            vertexNeigbours.put(fline(f3),Array(fline(f2)))
	          }
            //if ((verts2verts.contains(f3) && verts2verts(f3) == 0) || (verts2verts.contains(f2) && verts2verts(f2) == 0)) {
 	          //  println("" + fline(f3).toString + "->" + verts2verts(fline(f3)).mkString(" ") )
	          //  println("" + fline(f2).toString + "->" + verts2verts(fline(f2)).mkString(" ") )
            //}
	        }
        }
        faces += fline
      }
    }
  
    /*state = parseObject
		for(line <- Source.fromFile(filename).getLines())
		  state(line)
		*/
		//doSomethingWithTheRings(ma, verts, vertexNeigbours, objectName, simplifyLimit)
		
		/ **
		val alreadyProcessed = new HashSet[Int]()
    val foundSegments = new ArrayBuffer[Ring2D]()
    while (alreadyProcessed.size != verts.length) {
      
      //println("alreadyProcessed.size %d  verts.length %d (missing %s) ".format(alreadyProcessed.size, verts.length, 
      //    (0 to verts.length-1).filter(x => !alreadyProcessed.contains(x)).mkString(" ") ))
          
      for (i <- 0 to verts.length-1 ) {
	      if (!alreadyProcessed.contains(i) && verts2verts.contains(i)) {
	      	val currentSegments = new ArrayBuffer[Int]
	      	var nextV = i
	        while (nextV != -1 && !alreadyProcessed.contains(nextV)) {
	          alreadyProcessed += nextV
	          currentSegments += nextV
	          nextV = verts2verts(nextV).find(!alreadyProcessed.contains(_)).getOrElse(-1)
	        }
	      	if (nextV == -1){
	      	  //println(currentSegments.mkString(":"))
	      	  if (currentSegments(0) != currentSegments.last) {
	      	    // Tie up the loop
 	      	    currentSegments += currentSegments(0)
	      	    //println("closed the loop to %s".format(currentSegments.mkString(":")))
	      	  }
 	      	  //println(verts.map(x=>x.mkString(",")).mkString("\n"))
	      	  //println(currentSegments.mkString(" "))
            val foundVerts = currentSegments.map(x=>Array(verts(x)(0),verts(x)(1))).toArray.flatten
	      	  foundSegments += new Ring2D(ma, objectName, foundVerts, new Array[Ring2D](0), simplifyLimit)
	      	}
	      }
	    }
    }
    

    //val intermediate1 = foundSegments.map(x=>new Ring2D(ma, objectName, x.verts, convert(ma, objectName, x.subRings))).toArray
    //val intermediate2 = foundSegments.map(x=>new RingsInRings(x)).toArray
    sortOutInternals(ma,foundSegments.toArray)
    * /
		new ArrayBuffer[Ring2D].toArray
	}
  */
 
  def loadObj(filename: String):(Array[Array[Vec2DZ]],BoundingBoxDeprecaded) = {
		val allVerts = new ArrayBuffer[Array[Vec2DZ]];
		var verts = new ArrayBuffer[Vec2DZ];
		var vertexCounter = 0
		var alreadyStoredVertexCounter = 1 // just to compensate for the "off by one" counting in .obj files
		
		//val faces = new ArrayBuffer[Array[Int]];
		val vertexId2vertex = new HashMap[Int,Vec2DZ];
		val bb = new BoundingBoxMutableDeprecaded

		var objectName = "";		
		var state:(String)=>Unit = null
		
    def parseObject(line:String):Unit = {
      if (line.startsWith("o ")) {
        if (0!=verts.length){
          allVerts += verts.filter(x => x.edges.size >0).toArray
          alreadyStoredVertexCounter += vertexCounter
          verts = new ArrayBuffer[Vec2DZ];
          vertexCounter = 0
        }
        objectName = line.substring(2)
        //println(objectName)
      } else if (line.startsWith("v")) {
        state = parseVerts
        state(line)
      }
    }
		
		def parseVerts(line:String):Unit = {
      if (line.startsWith("v ")) {
   		  //println("%d %d: %s".format(verts.length, verts.length+alreadyStoredVertexCounter, line))
        
        val v3 = new Vec2DZ(line.split(" ").drop(1).map( x => x.toFloat ))
        bb.include(v3.getX, v3.getY, v3.getZ)
        v3.objIndex = alreadyStoredVertexCounter + vertexCounter
        //println("saving v as %d".format(v3.objIndex))
        vertexCounter+=1

        verts+=v3
      } else if (line.startsWith("f ")) {
		    state = parseFaces
		    state(line)
		  }
    }
		
		def parseFaces(line:String):Unit = {
 		  //println(line)
      if (line.startsWith("o ")) {
   		  //println("new object found:" + line)
        state = parseObject
		    state(line)
      } else if (line.startsWith("f ")) {
   		  //println(line)
   		  //println("%d %d: %s".format(verts.length, verts.length+alreadyStoredVertexCounter, line))

        val fline = line.split(" ").drop(1).map( x => x.toInt )
        for (f2 <- 0 to fline.length-2 ){
          for (f3 <- f2+1 to fline.length-1 ){

            if (fline(f2)< alreadyStoredVertexCounter || fline(f2) > verts.size-1+alreadyStoredVertexCounter){
              println("f2=%d fline(f2)=%d f3=%d fline(f3)=%d verts.size=%d".format(f2, fline(f2), f3, fline(f3), verts.size))
              println("line=%s".format(line))
              println("allVerts.length=%d, vertexCounter=%d alreadyStoredVertexCounter=%d".format(allVerts.length, vertexCounter, alreadyStoredVertexCounter))
	      	    println("ERROR: some faces are incorrect: %d ".format(fline(f2)))
	      	  }
            
            if (fline(f3)< alreadyStoredVertexCounter || fline(f3) > verts.size-1+alreadyStoredVertexCounter){
              println("f2=%d fline(f2)=%d f3=%d fline(f3)=%d verts.size=%d".format(f2, fline(f2), f3, fline(f3), verts.size))
              println("line=%s".format(line))
              println("allVerts.length=%d, vertexCounter=%d alreadyStoredVertexCounter=%d".format(allVerts.length, vertexCounter, alreadyStoredVertexCounter))
	      	    println("ERROR: some faces are incorrect: %d ".format(fline(f3)))
	      	  }
            
            assert(f2!=f3)
            assert(fline(f2)!=fline(f3))
            val F2=verts(fline(f2)-alreadyStoredVertexCounter) 
            val F3=verts(fline(f3)-alreadyStoredVertexCounter) 
            
            if(F3.objIndex!=fline(f3)){
              println("F3.objIndex!=fline(f3), alreadyStoredVertexCounter=%d, fline(f3)-alreadyStoredVertexCounter=%d".format(alreadyStoredVertexCounter, fline(f3)-alreadyStoredVertexCounter))
              println("F3.objIndex=%d fline(f3)=%d".format(F3.objIndex,fline(f3)))
              println("line=%s".format(line))
            }
            if(F2.objIndex!=fline(f2) ){
          	  println("F2.objIndex!=fline(f2), alreadyStoredVertexCounter=%d fline(f2)-alreadyStoredVertexCounter".format(alreadyStoredVertexCounter), fline(f2)-alreadyStoredVertexCounter)
              println("F2.objIndex=%d fline(f2)=%d".format(F2.objIndex,fline(f2)))
              println("line=%s".format(line))
            }
            F2.addEdge(F3)
            F3.addEdge(F2)
	        }
        }
        //faces += fline
      }
    }
		
    state = parseObject
		for(line <- Source.fromFile(filename).getLines()) {
		  //println(line)
		  state(line) 
		}
    
		if (0 != verts.size){
		  allVerts += verts.filter(x => x.edges.size >0).toArray
		} 
		(allVerts.toArray, bb.immutable)
	}
}
