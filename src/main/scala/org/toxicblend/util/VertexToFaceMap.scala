package org.toxicblend.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

/**
 * A hash container keyed by vertexId. Pointing to all faceIds that uses that vertexId
 */
class VertexToFaceMap {
  
  /**
   * VertexId and FaceId is as a type safety during development.
   * When the code is stable they can be exchanged for a simple Int
   */
  class IntId(val value:Int) {
    override def toString() = value.toString()
    def toInt() = value
  }
  case class VertexId(välue:Int) extends IntId(välue)
  case class FaceId(välue:Int) extends IntId(välue)

  /**
   * vertexId => Set[faceId...]
   */
  protected val vertex2facesMap = new HashMap[VertexId,ArrayBuffer[FaceId]]
  
  /**
   * faceId => Set[vertexId...]
   */
  protected val face2vertexMap = new HashMap[FaceId,ArrayBuffer[VertexId]]
  
  /**
   * A set containing all of the two-vertex edges we already have added. 
   * Tuple sorted : smallest first
   */
  protected val alreadyAddedEdges = new HashSet[(Int,Int)]
  
  @inline
  protected def sortTwoVertexEdge(a:VertexId,b:VertexId):(Int,Int) = {
    if (a.value < b.value) {
      (a.value, b.value)
    } else {
      (b.value, a.value)
    }
  } 
  
  @inline def vertexId2faceIds(vertexId:VertexId):IndexedSeq[FaceId] = vertex2facesMap(vertexId)
  @inline def faceId2vertices(faceId:FaceId):IndexedSeq[VertexId] = face2vertexMap(faceId)
  // not to be used in production, just for testing
  @inline def vertexId2faces(vertexId:Int):IndexedSeq[FaceId] = vertex2facesMap(new VertexId(vertexId))
  // not to be used in production, just for testing
  @inline def faceId2vertices(faceId:Int):IndexedSeq[VertexId] = face2vertexMap(new FaceId(faceId))
  
  /**
   * returns a list of every vertex that is only used in one face
   */
  def endVertices:Iterable[VertexId]  = {
    val rv =
    vertex2facesMap.filter(item => item._2.size==1 && face2vertexMap(item._2(0)).size==2).map(item => item._1)
    rv
  }
  
  /**
   * returns a list of every face that has more than two vertices (ngons and triangles)
   */
  def nGons:Iterable[FaceId] = {
    val rv =
    face2vertexMap.filter(item => item._2.size>2).map( item => item._1)
    rv
  }
  
  def intersectionVertices:Iterable[VertexId]  = {
    val rv =
    vertex2facesMap.filter(item => item._2.size > 2).map(item => item._1)
    rv
  }
  
  //@inline
  //def apply(vertexId:VertexId) = vertex2facesMap(vertexId)
  
  @inline
  def contains(vertexId:VertexId):Boolean = {
    vertex2facesMap contains vertexId
  }
  
  def containsFace(faceId:FaceId) = face2vertexMap.contains(faceId)
  def containsVertex(vertexId:VertexId) = vertex2facesMap.contains(vertexId)
  
  def add(vertexIds:IndexedSeq[VertexId], faceId:FaceId):VertexToFaceMap = {
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
          val tmp = new ArrayBuffer[VertexId] 
          face2vertexMap(faceId) = tmp
          tmp
        }
        
      vertexIds.foreach(vertexId=> {
        if (vertex2facesMap contains vertexId) {
          vertex2facesMap(vertexId) += faceId
        } else {
          vertex2facesMap(vertexId) = new ArrayBuffer[FaceId]() += faceId
        }
        vertexList += vertexId
      })
    }
    this
  }
  
  @inline
  def add(vertexId1:VertexId,vertexId2:VertexId,faceId:FaceId):VertexToFaceMap = {
    assert(vertexId1!=vertexId2)
    val key=sortTwoVertexEdge(vertexId1,vertexId2)
    if (!alreadyAddedEdges.contains(key)) {
      alreadyAddedEdges += key
      if (vertex2facesMap contains vertexId1) {
        vertex2facesMap(vertexId1) += faceId
      } else {
        vertex2facesMap(vertexId1) = new ArrayBuffer[FaceId]() += faceId
      }
      if (vertex2facesMap contains vertexId2) {
        vertex2facesMap(vertexId2) += faceId
      } else {
        vertex2facesMap(vertexId2) = new ArrayBuffer[FaceId]() += faceId
      }
      if (face2vertexMap contains faceId) {
        face2vertexMap(faceId) += vertexId1 += vertexId2
      } else {
        face2vertexMap(faceId) = new ArrayBuffer[VertexId](2) += vertexId1 += vertexId2
      }
    } else {
      println("Refusing to add edge: " + key + " more than once")
    }
    this
  }
  
  @inline
  def add(vertexId1:Int,vertexId2:Int,faceId:Int):VertexToFaceMap = {
    add(new VertexId(vertexId1),new VertexId(vertexId2),new FaceId(faceId))
  }
  
  def add(vertexIds:IndexedSeq[Int], faceId:Int):VertexToFaceMap = {
    if (2==vertexIds.size){
      add(vertexIds(0), vertexIds(1), faceId)
    }else {
      add(vertexIds.map(v => new VertexId(v)), new FaceId(faceId))
    }
  }

  /* 
   def removeNotWorking(faceId:FaceId) = {
    face2vertexMap(faceId).foreach( edge => {
      // vertex2facesMap(edge).remove(faceId) 
    })
    face2vertexMap.remove(faceId)
  }
  */
  
  /**
   * remove double faces. i.e. faces that contain the same vertices, just in another order
   */
  def removeDoubleFaces() {
    val facesToBeDeleted = new HashSet[FaceId]
    face2vertexMap.filter(x => x._2.size==2).foreach(item => {
      val faceId = item._1
      if (!facesToBeDeleted.contains(faceId)) {
        val memberVertices = item._2
        val sortedVertices = memberVertices.sortBy(i=>i.value)
        memberVertices.foreach(vertex => {
          val otherFaces = vertex2facesMap(vertex).filter(x=>x!=faceId && face2vertexMap(x).size==2)
          otherFaces.foreach(otherFace => {
            if (!facesToBeDeleted.contains(otherFace)) {
              val sortedOtherVertices = face2vertexMap(otherFace).sortBy(x=>x.value)
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
  def findVertexIdLineStrips():(IndexedSeq[IndexedSeq[VertexId]],IndexedSeq[IndexedSeq[VertexId]]) = {

    val visitedFaces = new HashSet[FaceId]
    // ngon return value, contains vertices that is not part of any line strip
    val ngrv = new ArrayBuffer[ArrayBuffer[VertexId]]
    // line strip return value
    val lsrv = new ArrayBuffer[ArrayBuffer[VertexId]]   
   
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
    def followSegment(fromVertexId:VertexId, dstVertexId:VertexId, moveAlongfaceId:FaceId, segment:ArrayBuffer[VertexId] ){
      
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
    
    def initiateFollowSegment(startVertexId:VertexId, moveAlongfaceId:FaceId, segment:ArrayBuffer[VertexId] ){
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
        val segment = new ArrayBuffer[VertexId]
        initiateFollowSegment(vertexId,faceIds(0), segment)
        lsrv += segment
      }
    })
    
    // try to find intersection vertices
    intersectionVertices.foreach(vertexId => {
      vertex2facesMap(vertexId).foreach(faceId => {
        if ( !visitedFaces.contains(faceId) ){
          val segment = new ArrayBuffer[VertexId]
          initiateFollowSegment(vertexId,faceId, segment)
          lsrv += segment
        }
      })
    })
    
    // the only thing left should now be linestrips forming loops with no other connections
    face2vertexMap.foreach(x => {
      if ( !visitedFaces.contains(x._1)) {
        assert(x._2.size == 2)
        val segment = new ArrayBuffer[VertexId]
        initiateFollowSegment(x._2(0),x._1, segment)
        lsrv += segment
      }
    })
    
    assert(visitedFaces.size == face2vertexMap.size)
    (ngrv,lsrv)
  }
}
