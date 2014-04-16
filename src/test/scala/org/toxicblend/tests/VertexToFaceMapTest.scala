package org.toxicblend.tests

import org.toxicblend.util.VertexToFaceMap

object VertexToFaceMapTest {

  def main(args: Array[String]): Unit = {
     val t = new VertexToFaceMap
     t.add(0,1,1)
     t.add(1,2,2)
     t.add(2,3,3)
     t.add(3,4,4)
     
     t.add(5,6,5)
     t.add(5,8,6)
     t.add(8,7,7)
     t.add(6,7,8)
     
     t.add(9,10,9)
     t.add(10,11,10)
     t.add(11,12,11)
     t.add(12,13,12)
     t.add(11,14,13)
     t.add(14,15,14)
     
     t.add(16,17,15)
     t.add(17,18,16)
     t.add(16,18,17)
     t.add(18,19,18)
     t.add(16,19,19)
     
     t.add(Array(20,21,22),20)
     
     println("endVertices:" + t.endVertices.mkString(","))
     //println("intersectionVertices:" + t.intersectionVertices.mkString(","))
     
     println("facesOfVertex(0):" + t.vertexId2faceIds(0).mkString(","))
     println("facesOfVertex(4):" + t.vertexId2faceIds(4).mkString(","))
     println("facesOfVertex(11):" + t.vertexId2faceIds(11).mkString(","))
     println("facesOfVertex(18):" + t.vertexId2faceIds(18).mkString(","))
     println("facesOfVertex(16):" + t.vertexId2faceIds(16).mkString(","))
     println("facesOfVertex(21):" + t.vertexId2faceIds(21).mkString(","))

     println("vertexOfFace(10):" + t.faceId2vertices(10).mkString(","))
     println("vertexOfFace(1):"  + t.faceId2vertices(1).mkString(","))
     println("vertexOfFace(13):" + t.faceId2vertices(13).mkString(","))
     println("vertexOfFace(11):" + t.faceId2vertices(11).mkString(","))
     println("vertexOfFace(19):" + t.faceId2vertices(19).mkString(","))
     
     val ls1 = t.findVertexIdLineStrips._1
     val ls2 = t.findVertexIdLineStrips._2

     println("ngons: " + ls1.map(a=>a.mkString(" (",",",")")).mkString("\n") )
     println("Line strips: " + ls2.map(a=>a.mkString(" (",",",")")).mkString("\n") )

  }
}