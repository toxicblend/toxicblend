package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlenderProtos.{Model,Face}
import toxi.geom.{Vec3D,LineStrip3D,AABB, Line3D}
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class LineStripConverter private (val lineStrips:Seq[LineStrip3D], val bounds:AABB, val name:String="") {
  
  protected class LineStripModelBuilder(val modelBuilder:Model.Builder) {
    var vertexIndex = 0
    def addVertex(vertex:Vec3D) = {
      val pbvertex = org.toxicblend.protobuf.ToxicBlenderProtos.Vertex.newBuilder()
      pbvertex.setId(vertexIndex)
      pbvertex.setX(vertex.x())
      pbvertex.setY(vertex.y())
      pbvertex.setZ(vertex.z())
      modelBuilder.addVertexes(pbvertex)
      vertexIndex += 1
      vertexIndex
    }
    
    def addVertexAndEdgeToPrevious(vertex:Vec3D) = {
      addVertex(vertex)
      val face = Face.newBuilder()
      face.addVertexes(vertexIndex -1) // vertexIndex -1 = this vertex
      face.addVertexes(vertexIndex -2) // vertexIndex -2 = previous vertex
      modelBuilder.addFaces(face)
    }
  }
    
  /**
   * Adds unique vertexes to the vertex list of the model builder, also updates the faces
   * TODO: fix the imperative:ness 
   */
  protected def addVertex(model:Model.Builder,face:Face.Builder,vmap:HashMap[Vec3D,Int], vertex:Vec3D) = {
    if (!vmap.contains(vertex)) {
      val index = vmap.size
      vmap(vertex) = index
   
      val pbvertex = org.toxicblend.protobuf.ToxicBlenderProtos.Vertex.newBuilder()
      pbvertex.setId(index)
      pbvertex.setX(vertex.x())
      pbvertex.setY(vertex.y())
      pbvertex.setZ(vertex.z())
      model.addVertexes(pbvertex)
    }
    val index = vmap(vertex)
    face.addVertexes(index)
  }
  
  /**
   * Adds unique vertexes to the vertex list of the model builder, also updates the faces
   * TODO: fix the imperative:ness 
   */
  protected def addEdgeNonUniqueVertex(model:Model.Builder,face:Face.Builder, line:Line3D, vIndex:Int) = {
     var index = vIndex
     val pbvertex = org.toxicblend.protobuf.ToxicBlenderProtos.Vertex.newBuilder()
     pbvertex.setId(index)
     pbvertex.setX(line.a.x())
     pbvertex.setY(line.a.y())
     pbvertex.setZ(line.a.z())
     model.addVertexes(pbvertex)
     face.addVertexes(index)
     
     index += 1
     pbvertex.setId(index)
     pbvertex.setX(line.b.x())
     pbvertex.setY(line.b.y())
     pbvertex.setZ(line.b.z())
     model.addVertexes(pbvertex)
     face.addVertexes(index)
     index += 1
     index
  }
  
  /**
   * Center the object and return a new instance
   * TODO implement
   */
  def center() = {
    LineStripConverter.this
  }
  
  /**
   * TODO: fix the imperative:ness 
   */  
  def toPBModel(uniqueVertexes:Boolean=true) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlenderProtos.Model.newBuilder()
    val vertexes = new ArrayBuffer[Vec3D]
    if (uniqueVertexes) {
      val vmap = new collection.mutable.HashMap[Vec3D,Int]()
      lineStrips.foreach(linestrip=>{
      
        linestrip.getSegments().foreach(s=>{
          val face = Face.newBuilder()
          addVertex(modelBuilder,face,vmap, s.a)
          addVertex(modelBuilder,face,vmap, s.b)
          modelBuilder.addFaces(face)
        })
        
      })
    } else {
      val lsmb = new LineStripModelBuilder(modelBuilder)
      lineStrips.foreach(linestrip=>{
        lsmb.addVertex(linestrip.getSegments().get(0).a)
        linestrip.getSegments().foreach(s => lsmb.addVertexAndEdgeToPrevious(s.b))
      })
    }
    modelBuilder.setName(name)
    _root_.scala.Predef.println(bounds)
    modelBuilder
  } 
  
  def println() {
    lineStrips.foreach(ls=>{
      ls.getVertices().foreach(v=>print(v))
      System.out.println("")
    })
  }
}

object LineStripConverter {
  
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbModel:Model):LineStripConverter = {
    val vertexes = new Array[Vec3D](pbModel.getVertexesList().size())
    val bounds = new AABB()
    for (v<-pbModel.getVertexesList()) {
      val vector = new Vec3D(v.getX,v.getY,v.getZ)
      vertexes(v.getId()) = vector
      bounds.growToContainPoint(vector)
    }
    
    val lineStrips = new ArrayBuffer[LineStrip3D]
    for (f<-pbModel.getFacesList()) {
      val lineStrip = new LineStrip3D()
      f.getVertexesList().foreach(v => lineStrip.add(vertexes(v)))
      //lineStrip.add(vertexes(f.getVertexes(0)))
      lineStrips.append(lineStrip)
    }
    new LineStripConverter(lineStrips, bounds, pbModel.getName)
  }
  
  /** 
   * Constructs from some LineStrip3D:s
   */
  def apply(lineStrips:Seq[LineStrip3D]) = {
    val bounds = new AABB() 
    lineStrips.foreach(ls=>ls.getSegments().foreach(s=>{
      bounds.growToContainPoint(s.a) 
      bounds.growToContainPoint(s.b)
    }))
    new LineStripConverter(lineStrips, bounds)
  } 
  
  /** 
   * Constructs from some LineStrip3D
   */
  def apply(lineStrip:LineStrip3D, name:String="") = {
    val bounds = new AABB() 
    lineStrip.getSegments().foreach(s=>{
      bounds.growToContainPoint(s.a) 
      bounds.growToContainPoint(s.b)
    })
    new LineStripConverter(Array(lineStrip), bounds, name)
  } 
}