package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.{Model,Face}
import toxi.geom.Vec2D
import toxi.geom.Vec3D
import toxi.geom.Matrix4f
import org.toxicblend.geometry.ProjectionPlane
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object Vertex2DPointCloudConverter {
  
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbModel:Model, ignoreAxis:ProjectionPlane.ProjectionPlane, applyWorldTransform:Boolean=false):Vertex2DPointCloudConverter = {
    val vertexesList = pbModel.getVerticesList()
    val v = new Array[Vec2D](vertexesList.size())
    val mConverter =  Matrix4fConverter(pbModel)
    
    //println("received " + vertexesList.size()  + " vertices")
    vertexesList.foreach (vertex => {
      val new3dVertex = new Vec3D(vertex.getX, vertex.getY, vertex.getZ)
      if (applyWorldTransform) mConverter.matrix.transformOne(new3dVertex)
         
      v(vertex.getId()) = ignoreAxis match {
        case ProjectionPlane.YZ_PLANE => new Vec2D(new3dVertex.y, new3dVertex.z)
        case ProjectionPlane.XZ_PLANE => new Vec2D(new3dVertex.x, new3dVertex.z)
        case ProjectionPlane.XY_PLANE => new Vec2D(new3dVertex.x, new3dVertex.y)
      }
    })
    new Vertex2DPointCloudConverter(v, ignoreAxis, pbModel.getName)
  }
  
  /** 
   * Constructs from one Buffer[Vec2D]
   */
  def apply(points:Array[Vec2D], ignoreAxis:ProjectionPlane.ProjectionPlane, name:String):Vertex2DPointCloudConverter = {
    new Vertex2DPointCloudConverter(points, ignoreAxis, name)
  } 
  
  /** 
   * Constructs from one java.util.List[Vec2D]
   */
  def apply(points:java.util.List[Vec2D], ignoreAxis:ProjectionPlane.ProjectionPlane, name:String):Vertex2DPointCloudConverter = {
    new Vertex2DPointCloudConverter(points.asScala.toArray, ignoreAxis, name)
  }
}

class Vertex2DPointCloudConverter private (val points:Array[Vec2D], val ignoreAxis:ProjectionPlane.ProjectionPlane, val name:String="") {
  
  protected class Vertex2DConverterHelper(val modelBuilder:Model.Builder, val finalTransformation:Option[Matrix4fConverter]) {
    val inverseFinalTransformation = 
      if (finalTransformation.isDefined) 
        Option[Matrix4f]({val m=new Matrix4f(finalTransformation.get.matrix); m.invert(); m} )
      else 
        None
    protected var vertexIndex = 0
    
    def addVertex(vertex:Vec2D) = {
      val pbvertex = org.toxicblend.protobuf.ToxicBlendProtos.Vertex.newBuilder()
      pbvertex.setId(vertexIndex)
      val vertex3d = ignoreAxis match {
        case ProjectionPlane.YZ_PLANE => new Vec3D(0f,vertex.x, vertex.y)
        case ProjectionPlane.XZ_PLANE => new Vec3D(vertex.x, 0f, vertex.y)
        case ProjectionPlane.XY_PLANE => new Vec3D(vertex.x, vertex.y, 0f)
      }
      if (inverseFinalTransformation.isDefined) {
        inverseFinalTransformation.get.transformOne(vertex3d)
      }
      pbvertex.setX(vertex3d.x)
      pbvertex.setY(vertex3d.y)
      pbvertex.setZ(vertex3d.z)
      modelBuilder.addVertices(pbvertex)
      vertexIndex += 1
      vertexIndex
    }
    
    def addVertexAndEdgeToPrevious(vertex:Vec2D) = {
      if (addVertex(vertex) > 1) {
        val face = Face.newBuilder()
        face.addVertices(vertexIndex-2) // vertexIndex -1 = this vertex
        face.addVertices(vertexIndex-1) // vertexIndex -2 = previous vertex
        modelBuilder.addFaces(face)
      }
    }
    
    def closeLoop() = {
      if (vertexIndex > 1) {
        val face = Face.newBuilder()
        face.addVertices(vertexIndex -1) // vertexIndex -1 = last vertex used
        face.addVertices(0) // first one
        modelBuilder.addFaces(face)
      }
    }
  }
  
  /**
   * Create a packet buffer model from this ArrayBuffer[Vec2D].
   * The result will be a list of 2D points with edges between each point (n, n+1)
   */  
  def toPBModel(buildEdge:Boolean=false, finalTransformation:Option[Matrix4fConverter] ) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder()
    modelBuilder.setName(name)
    val helper = new Vertex2DConverterHelper(modelBuilder, finalTransformation)
    if (buildEdge) {
      points.foreach(v => helper.addVertexAndEdgeToPrevious(v))
      helper.closeLoop()
    } else {
      points.foreach(v => helper.addVertex(v)) 
    }
    if (finalTransformation.isDefined) {
      modelBuilder.setWorldOrientation(finalTransformation.get.toPBModel)
    }
    modelBuilder
  } 
}


