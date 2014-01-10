package org.toxicblend.operations.projectionoutline

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer


/**
 *  Nothing to see here, just some leftover experimental code
 * 
class FaceFuser {
   * Awkward implementation of a "merge-faces-with-adjacent-edges" class. Work in progress
  class FaceFuser (val vertexes:ArrayBuffer[ReadonlyVec2D], val inFaces:ArrayBuffer[ArrayBuffer[Int]]) {
    protected val processedFaces = new HashSet[Int] // a set of those faces we are already done with
    protected val removedFaces = new HashSet[Int]   // a set of those faces that got merged in the process
    protected val edges2faces = new MultiHash()
    protected val face2Polygon2D = new HashMap[Int,Polygon2D]()
    
 
    protected def fuseEdges(currentFace:ArrayBuffer[Int], faceToMerge:ArrayBuffer[Int], edge:(Int,Int)) {
      if (faceToMerge.size == 2 && (
         ( faceToMerge(0)==edge._1 &&  faceToMerge(1)==edge._2 ) ||
         ( faceToMerge(0)==edge._2 &&  faceToMerge(1)==edge._1 )) ) {
         println("FuseEdges " + currentFace.mkString("{",",","}") + " ignoring " + faceToMerge.mkString("{",",","}") + " at " + edge)
      } else if (faceToMerge.size == 3 && (
         ( faceToMerge(0)==edge._1 &&  faceToMerge(1)==edge._2 && faceToMerge(2)==edge._1 ) ||
         ( faceToMerge(0)==edge._2 &&  faceToMerge(1)==edge._1 && faceToMerge(2)==edge._2 )) ) {
      
        println("FuseEdges " + currentFace.mkString("{",",","}") + " ignoring " + faceToMerge.mkString("{",",","}") + " at " + edge)
      } else {
            
        println("FuseEdges " + currentFace.mkString("{",",","}") + " with " + faceToMerge.mkString("{",",","}") + " at " + edge)
        SeqOperations.mergeLists(currentFace,faceToMerge.clone,edge)
        println("  result: " + currentFace.mkString("{",",","}") )
      }
    }
    
     * returns true if any of the points are inside the polygon
    protected def containsPoints(faceId:Int, points:Seq[Int], ignoreEdge:(Int,Int)):Boolean = {
      val polygon = if (face2Polygon2D contains faceId) 
          face2Polygon2D(faceId)
        else {
          System.err.println("bleh " + faceId) 
          return false
        }
      points.foreach(p => {
        if (p != ignoreEdge._1 && p != ignoreEdge._2) {
          if (inFaces(faceId) contains p){
            return true
          }
          if (polygon.containsPoint(vertexes(p) )) {
            return true
          }
        }
      })
      false
    }
    
     * Fuse a single face with others until there are no more identical edges to merge
    protected def fuseFace(faceId:Int) = {
           
      processedFaces += faceId
      val currentFace = inFaces(faceId)//.clone
      var currentFacePolygon = 
        if (face2Polygon2D contains faceId) 
          face2Polygon2D(faceId)
        else {
          val p = new Polygon2D(currentFace.map(v => vertexes(v)))
          face2Polygon2D(faceId) = p
          p
        }
      var changesMadeToThisFace = false
      do {
        changesMadeToThisFace = false
        currentFace.sliding(2).foreach(currentEdge => {
          val currentEdgeTuple = edges2faces.sortEdge((currentEdge(0), currentEdge(1)))
          if (! edges2faces.contains(currentEdgeTuple)) {
            edges2faces.add(currentEdgeTuple,faceId)
          }
          // TODO: merge all the processed faces together 
          // TODO; updated faces should update edges2faces
          edges2faces(currentEdgeTuple).foreach(f => {
            if (f!=faceId && !processedFaces.contains(f) && !removedFaces.contains(f) ) {
              if (!containsPoints( faceId, inFaces(f), currentEdgeTuple)) {
                processedFaces += f
                fuseEdges(currentFace, inFaces(f), currentEdgeTuple)
                removedFaces += f
                face2Polygon2D.remove(f)
                inFaces(f).clear
                edges2faces.remove(f)
                edges2faces.remove(faceId)
                currentFace.sliding(2).foreach(currentEdge => edges2faces.add(currentEdge, faceId))
                currentFacePolygon = new Polygon2D(currentFace.map(v => vertexes(v)))
                changesMadeToThisFace = true
              }
            }
          })
        })
      } while (changesMadeToThisFace)
      face2Polygon2D(faceId) = currentFacePolygon
      currentFace
    } 
  }
  
   * repeatedly merge faces that share one edge
  protected def fuseFaces(vertexes:ArrayBuffer[ReadonlyVec2D], inFaces:ArrayBuffer[ArrayBuffer[Int]]) = {
    val ff = new FaceFuser(vertexes, inFaces)
    ff.fuseFaces
  }
    def fuseFaces() = {
  
    removeArtifactEdges()
    
    (0 until faces.size).foreach (f => faces(f).sliding(2).foreach( ps => edges2faces.add((ps(0), ps(1)),f )))
    val returnFaces = new ArrayBuffer[ArrayBuffer[Int]]
    (0 until inFaces.size).map(f => {
      if (!removedFaces.contains(f)) {
        val faceValues = inFaces(f)
        if (faceValues.size > 2) { 
          if (faceValues.size > 3 || 
              faceValues(0)!=faceValues(1) && faceValues(0)!=faceValues(2))
           returnFaces += fuseFace(f)
        }
      }
    })
  
    // convert to Array
    returnFaces.toArray//map(x=>x.toArray).toArray
  } 
   * Removes faces like [x,y] or [x,y,x] if the (x,y) edge is already present in other faces 
  protected def removeArtifactEdges() {
    (0 until inFaces.size).foreach( faceIndex => {
      val face = inFaces(faceIndex)
      if (face.size == 1) {
        System.err.println("face:" + faceIndex + " had only one vertex")
        face.clear
      }
      if (face.size == 2) {
        val edge = edges2faces.sortEdge((face(0),face(1)))
        if (edges2faces.edge2facesMap.contains(edge)) {
          if (edges2faces.edge2facesMap(edge).size > 1) {
            println("face:" + faceIndex + " was removed " + face.mkString("(",",",")"))
            edges2faces.remove(faceIndex)
            face.clear
          }
        } else {
          println("face:" + faceIndex + " did not contain the edge " + edge + ". face was removed "+ face.mkString("(",",",")"))
          edges2faces.remove(faceIndex)
          face.clear 
        }
      } else if (face.size == 3 && face(0) == face(2) && face(0) != face(1)) {
        val edge = edges2faces.sortEdge((face(0),face(1)))
        if (edges2faces.edge2facesMap.contains(edge)) {
          if (edges2faces.edge2facesMap(edge).size > 1) {
            println("face:" + faceIndex + " was removed " + face.mkString("(",",",")"))
            edges2faces.remove(faceIndex)
            face.clear
          }
        } else {
          println("face:" + faceIndex + " did not contain the edge " + edge + ". face was removed "+ face.mkString("(",",",")"))
          edges2faces.remove(faceIndex)
          face.clear 
        }
      }
    })
  }  
      //mutableFaces
    if (useEdgeMerge) {
      rvMesh = fuseFaces  //.map(fa => ArrayBuffer(fa: _*)).toArray
      println("reduced :" + rvMesh.faces.map(x => x.mkString("(",", ",")")).mkString(", ") + " faces:" + mutableFaces.size) 
    }  
}

* 
*/ 

object nothingtofindhere {
}