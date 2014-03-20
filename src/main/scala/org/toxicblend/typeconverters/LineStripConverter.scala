package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.{Model,Face}
import toxi.geom.{ReadonlyVec3D,Vec3D,LineStrip3D,AABB, Line3D}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

/**
 * Constructs the packet buffer from a toxi.geom.LineStrip3D construct
 */
class LineStripConverter private (val lineStrips:Seq[LineStrip3D], val bounds:AABB, val name:String="") {
  
  protected class LineStripModelBuilder(val modelBuilder:Model.Builder) {
    var vertexIndex = 0
    def addVertex(vertex:Vec3D) = {
      val pbvertex = org.toxicblend.protobuf.ToxicBlendProtos.Vertex.newBuilder()
      pbvertex.setId(vertexIndex)
      pbvertex.setX(vertex.x())
      pbvertex.setY(vertex.y())
      pbvertex.setZ(vertex.z())
      modelBuilder.addVertices(pbvertex)
      vertexIndex += 1
      vertexIndex
    }
    
    def addVertexAndEdgeToPrevious(vertex:Vec3D) = {
      addVertex(vertex)
      val face = Face.newBuilder()
      face.addVertices(vertexIndex -1) // vertexIndex -1 = this vertex
      face.addVertices(vertexIndex -2) // vertexIndex -2 = previous vertex
      modelBuilder.addFaces(face)
    }
  }
    
  /**
   * Adds unique vertices to the vertex list of the model builder, also updates the faces
   */
  protected def addVertex(model:Model.Builder,face:Face.Builder,vmap:HashMap[Vec3D,Int], vertex:Vec3D) = {
    if (!vmap.contains(vertex)) {
      val index = vmap.size
      vmap(vertex) = index
   
      val pbvertex = org.toxicblend.protobuf.ToxicBlendProtos.Vertex.newBuilder()
      pbvertex.setId(index)
      pbvertex.setX(vertex.x())
      pbvertex.setY(vertex.y())
      pbvertex.setZ(vertex.z())
      model.addVertices(pbvertex)
    }
    val index = vmap(vertex)
    face.addVertices(index)
  }
  
  /**
   * Adds unique vertices to the vertex list of the model builder, also updates the faces
   */
  protected def addEdgeNonUniqueVertex(model:Model.Builder,face:Face.Builder, line:Line3D, vIndex:Int) = {
     var index = vIndex
     val pbvertex = org.toxicblend.protobuf.ToxicBlendProtos.Vertex.newBuilder()
     pbvertex.setId(index)
     pbvertex.setX(line.a.x())
     pbvertex.setY(line.a.y())
     pbvertex.setZ(line.a.z())
     model.addVertices(pbvertex)
     face.addVertices(index)
     
     index += 1
     pbvertex.setId(index)
     pbvertex.setX(line.b.x())
     pbvertex.setY(line.b.y())
     pbvertex.setZ(line.b.z())
     model.addVertices(pbvertex)
     face.addVertices(index)
     index += 1
     index
  }
  
  /**
   * Center the object (modifies the vertices in place)
   */
  def center(cursorPos:ReadonlyVec3D) = {
    val newCenter = cursorPos.sub(bounds)
    lineStrips.foreach( ls => ls.getVertices.foreach(v => v.addSelf(newCenter)))
    this
  }
  
  /**
   * generates a packet buffer model of these line strips
   */  
  def toPBModel(uniqueVertices:Boolean=true) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder()
    val vertices = new ArrayBuffer[Vec3D]
    if (uniqueVertices) {
      val vmap = new collection.mutable.HashMap[Vec3D,Int]()
      lineStrips.foreach(linestrip=>{
        linestrip.getVertices.sliding(2).foreach(s=>{
          val face = Face.newBuilder()
          addVertex(modelBuilder,face,vmap, s(0))
          addVertex(modelBuilder,face,vmap, s(1))
          modelBuilder.addFaces(face)
        })
        
      })
    } else {
      val lsmb = new LineStripModelBuilder(modelBuilder)
      lineStrips.foreach(linestrip=>{
        lsmb.addVertex(linestrip.getVertices.head)
        linestrip.getVertices.tail.foreach(s => lsmb.addVertexAndEdgeToPrevious(s))
      })
    }
    modelBuilder.setName(name)
    modelBuilder
  } 
}

object LineStripConverter {
  
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbModel:Model, useWorldCoordinares:Boolean):LineStripConverter = {
   
    val worldTransformation = if (useWorldCoordinares && pbModel.hasWorldOrientation) Option(Matrix4x4Converter(pbModel.getWorldOrientation)) else None
    val vertices = new Array[Vec3D](pbModel.getVerticesList.size)
    if (worldTransformation.isDefined) {
      val wtransform = worldTransformation.get.matrix
      val aabb = {
        val firstVertexOpt = Mesh3DConverter.getFirstVertex(pbModel)
        if (firstVertexOpt.isDefined) {
          new AABB(wtransform.applyToSelf(firstVertexOpt.get),0f)
        } else {
          new AABB // no vertices, aabb will have origin at origo
        }
      }
      for (v<-pbModel.getVerticesList()) {
        val vector = wtransform.applyToSelf(new Vec3D(v.getX,v.getY,v.getZ))
        vertices(v.getId()) = vector
        aabb.growToContainPoint(vector)
      }
      
      val lineStrips = new ArrayBuffer[LineStrip3D]
      for (f<-pbModel.getFacesList()) {
        val lineStrip = new LineStrip3D
        f.getVerticesList.foreach(v => lineStrip.add(vertices(v)))
        lineStrips.append(lineStrip)
      }
      new LineStripConverter(lineStrips, aabb, pbModel.getName)
    } else {
      // no worldTransformation
      
      val firstVertexOpt = Mesh3DConverter.getFirstVertex(pbModel)
      val aabb = if (firstVertexOpt.isDefined) {
        new AABB(firstVertexOpt.get,0f)
      } else {
        new AABB // no vertices, aabb will have origin at origo
      }
      
      for (v<-pbModel.getVerticesList()) {
        val vector = new Vec3D(v.getX,v.getY,v.getZ)
        vertices(v.getId) = vector
        aabb.growToContainPoint(vector)
      }
      
      val lineStrips = new ArrayBuffer[LineStrip3D]
      for (f<-pbModel.getFacesList()) {
        val lineStrip = new LineStrip3D()
        f.getVerticesList().foreach(v => lineStrip.add(vertices(v)))
        lineStrips.append(lineStrip)
      }
      new LineStripConverter(lineStrips, aabb, pbModel.getName)
    }
  }
   
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbModel:Model):LineStripConverter = apply(pbModel,false)
  
  /** 
   * Constructs from some LineStrip3D:s
   */
  def apply(lineStrips:Seq[LineStrip3D], name:String) = {
    if (lineStrips.size > 0) {
	    val bounds = new AABB(lineStrips.head.head,0f) 
	    lineStrips.foreach(ls => ls.getVertices.foreach(v => bounds.growToContainPoint(v)))
	    new LineStripConverter(lineStrips, bounds, name)
    } else {
      // this should be an error state, really
      new LineStripConverter(new ArrayBuffer[LineStrip3D], new AABB, name)      
    }
  } 
  
  /** 
   * Constructs from a LineStrip3D
   */
  def apply(lineStrip:LineStrip3D, name:String="") = {
    val bounds = if (lineStrip.getVertices.size <= 0) {
      new AABB() 
    } else {
      new AABB(lineStrip.getVertices.head,0f)
    }
    lineStrip.getVertices.foreach(v => bounds.growToContainPoint(v))
    new LineStripConverter(Array(lineStrip), bounds, name)
  } 
}