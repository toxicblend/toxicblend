package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import toxi.geom.Vec3D
import toxi.geom.Matrix4x4
import org.toxicblend.geometry.ProjectionPlane
import org.toxicblend.geometry.Mesh2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import org.toxicblend.vecmath.Vec2D
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
 * Handle every mesh as a set of 2D polygons
 * (one axle is not used)
 */
class Mesh2DConverter private (val mesh2d:Mesh2D, val projectionPlane:ProjectionPlane.ProjectionPlane, val name:String="") 
{
  
  protected class Vertex3DConverterHelper(val modelBuilder:Model.Builder, val finalTransformation:Option[Matrix4x4Converter]) {
    val inverseFinalTransformation = 
      if (finalTransformation.isDefined) 
        Option[Matrix4x4]({val m=new Matrix4x4(finalTransformation.get.matrix); m.invert(); m} )
      else 
        None
    protected var vertexIndex = 0
    
    def addVertex(vertex:Vec2D) = {
      val pbvertex = org.toxicblend.protobuf.ToxicBlendProtos.Vertex.newBuilder()
      pbvertex.setId(vertexIndex)
      val vertex3d = projectionPlane match {
        case ProjectionPlane.YZ_PLANE => new Vec3D(0f,vertex.x.toFloat, vertex.y.toFloat)
        case ProjectionPlane.XZ_PLANE => new Vec3D(vertex.x.toFloat, 0f, vertex.y.toFloat)
        case ProjectionPlane.XY_PLANE => new Vec3D(vertex.x.toFloat, vertex.y.toFloat, 0f)
      }
      if (inverseFinalTransformation.isDefined) {
        inverseFinalTransformation.get.applyToSelf(vertex3d)
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
    
    def addFace(face:IndexedSeq[Int]) = {
      val faceBuilder = Face.newBuilder()
      face.foreach(f => faceBuilder.addVertices(f))
      modelBuilder.addFaces(faceBuilder)
    }
    
    /**
     * convenience operator to add a single edge
     */
    def addFace(edgeVertex1:Int, edgeVertex2:Int) = {
      val faceBuilder = Face.newBuilder()
      faceBuilder.addVertices(edgeVertex1)
      faceBuilder.addVertices(edgeVertex2)
      modelBuilder.addFaces(faceBuilder)
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
   * Create a packet buffer model from this Mesh2D.
   * The result will be a list of 2D points with edges between each point (n, n+1)
   */  
  def toPBModel(noFaceOnlyEdges:Boolean=false, finalTransformation:Option[Matrix4x4Converter] ) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder()
    modelBuilder.setName(name)
    val helper = new Vertex3DConverterHelper(modelBuilder, finalTransformation)
    mesh2d.vertices.foreach(v => helper.addVertex(v)) 
    if (noFaceOnlyEdges)
      mesh2d.faces.foreach(f => {
        f.sliding(2).foreach(e => 
          helper.addFace(e.to)
        )
        // close the implicitly closed loop, but only if it's a real loop, not just an edge
        if (f.size>2) helper.addFace(f(0),f.last)
      })
     else
        mesh2d.faces.foreach(f => helper.addFace(f))

    if (finalTransformation.isDefined) {
      modelBuilder.setWorldOrientation(finalTransformation.get.toPBModel)
    }
    modelBuilder
  }
  
}

object Mesh2DConverter {
  
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbModel:Model, projectionPlane:ProjectionPlane.ProjectionPlane, applyWorldTransform:Boolean=false):Mesh2DConverter = {
    val vertexList = pbModel.getVerticesList()
    val points2D = new Array[Vec2D](vertexList.size).to[ArrayBuffer]
    val matrixConverter =  Matrix4x4Converter(pbModel)
    
    //println("received " + verticesList.size()  + " vertices")
    vertexList.foreach (pbVertex => {
      val new3dVertex = new Vec3D(pbVertex.getX, pbVertex.getY, pbVertex.getZ)
      if (applyWorldTransform) matrixConverter.matrix.applyToSelf(new3dVertex)
         
      points2D(pbVertex.getId()) = projectionPlane match {
        case ProjectionPlane.YZ_PLANE => Vec2D(new3dVertex.y, new3dVertex.z)
        case ProjectionPlane.XZ_PLANE => Vec2D(new3dVertex.x, new3dVertex.z)
        case ProjectionPlane.XY_PLANE => Vec2D(new3dVertex.x, new3dVertex.y)
      }
    })
    
    val faces2D = pbModel.getFacesList().map(f => {
      f.getVerticesList().map( p => p.toInt ).to[ArrayBuffer]  
    }).to[ArrayBuffer]
    new Mesh2DConverter(Mesh2D(points2D,faces2D) , projectionPlane, pbModel.getName)
  }
  
  /** 
   * Constructs from one Buffer[Vec2D]
   */
  def apply(points:ArrayBuffer[Vec2D], faces:ArrayBuffer[ArrayBuffer[Int]], projectionPlane:ProjectionPlane.ProjectionPlane, name:String, useEdgeMerge:Boolean):Mesh2DConverter = {
    new Mesh2DConverter(Mesh2D(points,faces), projectionPlane, name)
  } 
}
