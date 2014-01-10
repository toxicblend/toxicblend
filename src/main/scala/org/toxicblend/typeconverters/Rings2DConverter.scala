package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlenderProtos.Model
import org.toxicblend.protobuf.ToxicBlenderProtos.Face
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Vec2D
import toxi.geom.Matrix4f
import toxi.geom.Rect
import org.toxicblend.geometry.ProjectionPlane
import org.toxicblend.geometry.Rings2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
 * Handle every mesh as a set of 2D polygons
 * It stores the mesh in a Rings2D object
 * This class and companion object is practically identical to Mesh2DConverter. TODO: fix that
 * (one axle is not used)
 */
class Rings2DConverter private (val mesh2d:Rings2D, val projectionPlane:ProjectionPlane.ProjectionPlane, val name:String="") 
{
  
  protected class Vertex3DConverterHelper(val modelBuilder:Model.Builder, val finalTransformation:Option[Matrix4fConverter]) {
    val inverseFinalTransformation = 
      if (finalTransformation.isDefined) 
        Option[Matrix4f]({val m=new Matrix4f(finalTransformation.get.matrix); m.invert(); m} )
      else 
        None
    protected var vertexIndex = 0
    
    def addVertex(vertex:ReadonlyVec2D) = {
      val pbvertex = org.toxicblend.protobuf.ToxicBlenderProtos.Vertex.newBuilder()
      pbvertex.setId(vertexIndex)
      val vertex3d = projectionPlane match {
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
      modelBuilder.addVertexes(pbvertex)
      vertexIndex += 1
      vertexIndex
    }
    
    def addVertexAndEdgeToPrevious(vertex:ReadonlyVec2D) = {
      if (addVertex(vertex) > 1) {
        val face = Face.newBuilder()
        face.addVertexes(vertexIndex-2) // vertexIndex -1 = this vertex
        face.addVertexes(vertexIndex-1) // vertexIndex -2 = previous vertex
        modelBuilder.addFaces(face)
      }
    }
    
    def addFace(face:IndexedSeq[Int]) = {
      val faceBuilder = Face.newBuilder()
      face.foreach(f => faceBuilder.addVertexes(f))
      modelBuilder.addFaces(faceBuilder)
    }
    
    /**
     * convenience operator to add a single edge
     */
    def addFace(edgeVertex1:Int, edgeVertex2:Int) = {
      val faceBuilder = Face.newBuilder()
      faceBuilder.addVertexes(edgeVertex1)
      faceBuilder.addVertexes(edgeVertex2)
      modelBuilder.addFaces(faceBuilder)
    }
    
    def closeLoop() = {
      if (vertexIndex > 1) {
        val face = Face.newBuilder()
        face.addVertexes(vertexIndex -1) // vertexIndex -1 = last vertex used
        face.addVertexes(0) // first one
        modelBuilder.addFaces(face)
      }
    }
  }
  
  /**
   * Create a packet buffer model from this Rings2D.
   * The result will be a list of 2D points with edges between each point (n, n+1)
   */  
  def toPBModel(noFaceOnlyEdges:Boolean=false, finalTransformation:Option[Matrix4fConverter] ) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlenderProtos.Model.newBuilder()
    modelBuilder.setName(name)
    val helper = new Vertex3DConverterHelper(modelBuilder, finalTransformation)
    mesh2d.vertexes.foreach(v => helper.addVertex(v)) 
    if (noFaceOnlyEdges)
      mesh2d.faces.foreach(f => {
        f.sliding(2).foreach(e => 
          helper.addFace(e.to)
        )
        helper.addFace(f(0),f.last)
      })
     else
        mesh2d.faces.foreach(f => helper.addFace(f))

    if (finalTransformation.isDefined) {
      modelBuilder.setWorldOrientation(finalTransformation.get.toPBModel)
    }
    modelBuilder
  } 
  
}

object Rings2DConverter {
  /** 
   * Constructs from a packet buffer model
   */
  def apply(pbModel:Model, projectionPlane:ProjectionPlane.ProjectionPlane, applyWorldTransform:Boolean=false) = {
    
    val vertexesList = pbModel.getVertexesList()
    val points2D = new Array[ReadonlyVec2D](vertexesList.size).to[ArrayBuffer] // buffer initiated and filled
    val matrixConverter =  Matrix4fConverter(pbModel)
    
    println("Rings2DConverter received " + vertexesList.size()  + " vertices")
    val aabb = new Rect
    vertexesList.foreach (pbVertex => {
      val new3dVertex = new Vec3D(pbVertex.getX, pbVertex.getY, pbVertex.getZ)
      if (applyWorldTransform) matrixConverter.matrix.transformOne(new3dVertex)
         
      val new2dVertex = projectionPlane match {
        case ProjectionPlane.YZ_PLANE => new Vec2D(new3dVertex.y, new3dVertex.z)
        case ProjectionPlane.XZ_PLANE => new Vec2D(new3dVertex.x, new3dVertex.z)
        case ProjectionPlane.XY_PLANE => new Vec2D(new3dVertex.x, new3dVertex.y)
      }
      aabb.growToContainPoint(new2dVertex)
      points2D(pbVertex.getId()) = new2dVertex
    })
    
    println("aabb getBottomLeft= " + aabb.getBottomLeft() + " getTopRight=" + aabb.getTopRight())
    
    val faces2D = pbModel.getFacesList().map(f => {
      f.getVertexesList().map( p => p.toInt ).to[ArrayBuffer]  
    }).to[ArrayBuffer]
 
    new Rings2DConverter( Rings2D(points2D,faces2D), projectionPlane, pbModel.getName)
  }
  
  /** 
   * Constructs from one Buffer[Vec2D]
   */
  def Rings2DConverter(points:ArrayBuffer[ReadonlyVec2D], faces:ArrayBuffer[ArrayBuffer[Int]], projectionPlane:ProjectionPlane.ProjectionPlane, name:String, useEdgeMerge:Boolean) = {
    new Rings2DConverter( Rings2D(points,faces), projectionPlane, name)
  } 
}

