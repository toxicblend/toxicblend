package org.toxicblend.tests

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import toxi.geom.ReadonlyVec2D
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni
import org.toxicblend.operations.boostmedianaxis.Ring2D

trait RingParser {  
  
  
  /**
   * 
   */
  def createAndSortRingsJunk(ma:MedianAxisJni, verts: ArrayBuffer[Array[ReadonlyVec2D]], verts2verts: HashMap[Int,Array[Int]],
                         objectName:String, simplifyLimit:Float) = {
    
    val alreadyProcessed = new HashSet[Int]()
    val foundSegments = new ArrayBuffer[Ring2D]()
    while (alreadyProcessed.size != verts.length) {
      
      //println("alreadyProcessed.size %d  verts.length %d (missing %s) ".format(alreadyProcessed.size, verts.length, 
      //    (0 to verts.length-1).filter(x => !alreadyProcessed.contains(x)).mkString(" ") ))
          
      (0 until verts.length).foreach(i => {
        if (!alreadyProcessed.contains(i) && verts2verts.contains(i)) {
          val currentSegments = new ArrayBuffer[ReadonlyVec2D]
          var nextV = i
          while (nextV != -1 && !alreadyProcessed.contains(nextV)) {
            alreadyProcessed += nextV
//TODO            currentSegments += new Vec2D(verts(nextV).x, verts(nextV).y)
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
            //val flattenedVerts = currentSegments.map(x=>Array(verts(x)(0),verts(x)(1))).toArray.flatten
  //TODO          foundSegments += new Ring2D(ma, objectName, verts, new Array[Ring2D](0), simplifyLimit)
          }
        }
      }
    )}

    //val intermediate1 = foundSegments.map(x=>new Ring2D(ma, objectName, x.verts, convert(ma, objectName, x.subRings))).toArray
    //val intermediate2 = foundSegments.map(x=>new RingsInRings(x)).toArray
    //sortOutInternals(ma,foundSegments.toArray)
  }
}