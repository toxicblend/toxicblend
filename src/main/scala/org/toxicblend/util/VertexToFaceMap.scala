package org.toxicblend.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

/**
 * A hash container keyed by vertexId. Pointing to all faceIds that uses that vertexId
 */
class VertexToFaceMap {
  
  /**
   * vertexId => Set[faceId...]
   */
  protected val vertex2facesMap = new HashMap[Int,ArrayBuffer[Int]]
  
  /**
   * faceId => Set[vertexId...]
   */
  protected val face2vertexMap = new HashMap[Int,ArrayBuffer[Int]]
  
  /**
   * A set containing all of the two-vertex edges we already have added. 
   * Tuple sorted : smallest first
   */
  protected val alreadyAddedEdges = new HashSet[(Int,Int)]
  
  @inline
  protected def sortTwoVertexEdge(a:Int,b:Int):(Int,Int) = {
    if (a < b) (a, b)
    else (b, a)
  } 
  
  @inline def vertexId2faceIds(vertexId:Int):IndexedSeq[Int] = vertex2facesMap(vertexId)
  @inline def faceId2vertices(faceId:Int):IndexedSeq[Int] = face2vertexMap(faceId)
 
  /**
   * returns a list of every vertex that is only used in one face
   */
  def endVertices:Iterable[Int] = {
    val rv = vertex2facesMap.filter(item => item._2.size==1 && face2vertexMap(item._2(0)).size==2).map(item => item._1)
    rv
  }
  
  /**
   * returns a list of every face that has more than two vertices (ngons and triangles)
   */
  def nGons:Iterable[Int] = {
    val rv = face2vertexMap.filter(item => item._2.size>2).map( item => item._1)
    rv
  }
  
  def intersectionVertices:Iterable[Int]  = {
    val rv = vertex2facesMap.filter(item => item._2.size > 2).map(item => item._1)
    rv
  }

  @inline
  def contains(vertexId:Int):Boolean = {
    vertex2facesMap contains vertexId
  }
  
  def containsFace(faceId:Int) = face2vertexMap.contains(faceId)
  def containsVertex(vertexId:Int) = vertex2facesMap.contains(vertexId)
  
  def add(vertexIds:IndexedSeq[Int], faceId:Int):VertexToFaceMap = {
    if(vertexIds.distinct.size!=vertexIds.size){
      println("VertexToFaceMap found clone vertices, trying to cope")
    }
    if(2==vertexIds.size) {
      add(vertexIds(0),vertexIds(1),faceId)
    } else {
      val vertexList = 
        if (face2vertexMap contains faceId) {
          face2vertexMap(faceId)
        } else {
          val tmp = new ArrayBuffer[Int] 
          face2vertexMap(faceId) = tmp
          tmp
        }
        
      vertexIds.foreach(vertexId=> {
        if (vertex2facesMap contains vertexId) {
          vertex2facesMap(vertexId) += faceId
        } else {
          vertex2facesMap(vertexId) = new ArrayBuffer[Int]() += faceId
        }
        vertexList += vertexId
      })
    }
    this
  }
  
  @inline
  def add(vertexId1:Int, vertexId2:Int, faceId:Int):VertexToFaceMap = {
    assert(vertexId1!=vertexId2)
    val key=sortTwoVertexEdge(vertexId1,vertexId2)
    if (!alreadyAddedEdges.contains(key)) {
      alreadyAddedEdges += key
      if (vertex2facesMap contains vertexId1) {
        vertex2facesMap(vertexId1) += faceId
      } else {
        vertex2facesMap(vertexId1) = new ArrayBuffer[Int]() += faceId
      }
      if (vertex2facesMap contains vertexId2) {
        vertex2facesMap(vertexId2) += faceId
      } else {
        vertex2facesMap(vertexId2) = new ArrayBuffer[Int]() += faceId
      }
      if (face2vertexMap contains faceId) {
        face2vertexMap(faceId) += vertexId1 += vertexId2
      } else {
        face2vertexMap(faceId) = new ArrayBuffer[Int](2) += vertexId1 += vertexId2
      }
    } else {
      println("Refusing to add edge: " + key + " more than once")
    }
    this
  }
  
  /**
   * remove double faces. i.e. faces that contain the same vertices, just in another order
   * TODO: this seems broken, fix it
   */
  def removeDoubleFacesBroken = {
    val facesToBeDeleted = new HashSet[Int]
    face2vertexMap.filter(x => x._2.size==2).foreach(item => {
      val faceId = item._1
      if (!facesToBeDeleted.contains(faceId)) {
        val memberVertices = item._2
        val sortedVertices = memberVertices.sortBy(i=>i)
        memberVertices.foreach(vertex => {
          val otherFaces = vertex2facesMap(vertex).filter(x=>x!=faceId && face2vertexMap(x).size==2)
          otherFaces.foreach(otherFace => {
            if (!facesToBeDeleted.contains(otherFace)) {
              val sortedOtherVertices = face2vertexMap(otherFace).sortBy(x=>x)
              if (sortedOtherVertices.sameElements(sortedVertices)){
                facesToBeDeleted += otherFace
              }
            }
          })
        })
      }
    })
    facesToBeDeleted.foreach(faceId => {
      val affectedVertices = face2vertexMap(faceId)
      //affectedVertices.foreach(vertex => vertex2facesMap(vertex).re(n))
      
    })
  }
  
  /**
   * returns a tuple containing lists of vertices that :
   *   is part of ngons (3 or more vertices that is part of a single face)  == ._1
   *   is forming a line strip (vertices connected to only two edges)       == ._2
   *       Note that the start and end points of the line strip can be connected to any number of vertices
   */
  def findVertexIdLineStrips():(IndexedSeq[IndexedSeq[Int]],IndexedSeq[IndexedSeq[Int]]) = {

    val visitedFaces = new HashSet[Int]
    // ngon return value, contains vertices that is not part of any line strip
    val ngrv = new ArrayBuffer[ArrayBuffer[Int]]
    // line strip return value
    val lsrv = new ArrayBuffer[ArrayBuffer[Int]]   
   
    val t = intersectionVertices.size
    
    //println("There are " + t +" intersection vertices.")    
    //println("There are " + endVertices.size +" end vertices.") 
    //println("There are " + vertex2facesMap.size +" vertices.")
    //println("There are " + face2vertexMap.size +" faces/edges.")
    
    //println("intersection vertices: " + intersectionVertices.mkString(",") )
    //println("end vertices: " + endVertices.mkString(",") )
    //println()
    
    /**
     * move along a face 'moveAlongfaceId', to 'dstVertexId'
     * 'fromVertexId' is the vertex we came from
     * stop when an end point or a intersection point is reached
     * append 'dstVertexId' to 'segment'
     */
    //@tailrec
    def followSegment(fromVertexId:Int, dstVertexId:Int, moveAlongfaceId:Int, segment:ArrayBuffer[Int] ){
      
      val nextFaceIds = this.vertexId2faceIds(dstVertexId).filter(f => f!=moveAlongfaceId && !visitedFaces.contains(f))
      if (nextFaceIds.size != 1 || this.vertexId2faceIds(dstVertexId).size > 2 ) {
        visitedFaces += moveAlongfaceId
        //println(" nextFaceIds = " +  nextFaceIds.mkString(",")  )
        segment += dstVertexId
      } else {
        visitedFaces += moveAlongfaceId
        segment += dstVertexId
        val nextVertexIds = this.faceId2vertices(nextFaceIds(0)).filter(v => v!=dstVertexId) 
        followSegment(dstVertexId, nextVertexIds(0),nextFaceIds(0), segment)
      }
    }
    
    def initiateFollowSegment(startVertexId:Int, moveAlongfaceId:Int, segment:ArrayBuffer[Int] ){
      val dstVertexIds = faceId2vertices(moveAlongfaceId).filter( vertex => vertex!=startVertexId)
      if (dstVertexIds.size == 1) {
        segment += startVertexId
        followSegment(startVertexId,dstVertexIds(0),moveAlongfaceId, segment)
      } else {
        System.err.println("initiateFollowSegment: debug me")
      }
    }
    
    // loop over every ngon face and store the vertices in ngrv 
    nGons.foreach( faceId => {
      if (! visitedFaces.contains(faceId) ) {
        ngrv += face2vertexMap(faceId) 
        visitedFaces += faceId
      }
    })
    
    // loop over every vertex that is connected by a single face
    endVertices.foreach(vertexId => {
      val faceIds = this.vertex2facesMap(vertexId)
      assert(1==faceIds.size)
      
      if (! visitedFaces.contains(faceIds(0)) ){
        val segment = new ArrayBuffer[Int]
        initiateFollowSegment(vertexId,faceIds(0), segment)
        lsrv += segment
      }
    })
    
    // try to find intersection vertices
    intersectionVertices.foreach(vertexId => {
      vertex2facesMap(vertexId).foreach(faceId => {
        if ( !visitedFaces.contains(faceId) ){
          val segment = new ArrayBuffer[Int]
          initiateFollowSegment(vertexId,faceId, segment)
          lsrv += segment
        }
      })
    })
    
    // the only thing left should now be linestrips forming loops with no other connections
    face2vertexMap.foreach(x => {
      if ( !visitedFaces.contains(x._1)) {
        assert(x._2.size == 2)
        val segment = new ArrayBuffer[Int]
        initiateFollowSegment(x._2(0),x._1, segment)
        lsrv += segment
      }
    })
    
    assert(visitedFaces.size == face2vertexMap.size)
    (ngrv,lsrv)
  }
}
