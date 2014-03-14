package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Vec2D
import toxi.geom.Matrix4x4
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
class Rings2DConverter private (val mesh2d:Rings2D, val projectionPlane:ProjectionPlane.ProjectionPlane, val name:String="") {
  
  /**
   * Create a packet buffer model from this Rings2D.
   * The result will be a list of 2D points with edges between each point (n, n+1)
   */  
  def toPBModel(noFaceOnlyEdges:Boolean=false, finalTransformation:Option[Matrix4x4Converter] ) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder()
    modelBuilder.setName(name)
    val helper = new Vertex3DConverterHelper(modelBuilder, projectionPlane, finalTransformation)
    mesh2d.vertices.foreach(v => helper.addVertex(v)) 
    if (noFaceOnlyEdges)
      mesh2d.faces.foreach(f => {
        f.sliding(2).foreach(e => 
          helper.addFace(e.to)
        )
        if (f.size>2) {
          helper.addFace(f(0),f.last)
        }
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
    
    val vertexList = pbModel.getVerticesList()
    val points2D = new Array[ReadonlyVec2D](vertexList.size).to[ArrayBuffer] // buffer initiated and filled
    val matrixConverter =  Matrix4x4Converter(pbModel)
    
    //println("Rings2DConverter received " + vertexList.size()  + " vertices")
    var aabb:Option[Rect] = None
    vertexList.foreach (pbVertex => {
      val new3dVertex = new Vec3D(pbVertex.getX, pbVertex.getY, pbVertex.getZ)
      if (applyWorldTransform) matrixConverter.matrix.applyToSelf(new3dVertex)
      
      val new2dVertex = projectionPlane match {
        case ProjectionPlane.YZ_PLANE => new Vec2D(new3dVertex.y, new3dVertex.z)
        case ProjectionPlane.XZ_PLANE => new Vec2D(new3dVertex.x, new3dVertex.z)
        case ProjectionPlane.XY_PLANE => new Vec2D(new3dVertex.x, new3dVertex.y)
      }
      if (aabb.isEmpty) {
        aabb = Option(new Rect(new2dVertex,new2dVertex))
      }
      aabb.get.growToContainPoint(new2dVertex)
      points2D(pbVertex.getId()) = new2dVertex
    })
    
    //println("aabb getBottomLeft= " + aabb.get.getBottomLeft + " getTopRight=" + aabb.get.getTopRight)
    
    val faces2D = pbModel.getFacesList().map(f => {
      f.getVerticesList().map( p => p.toInt ).to[ArrayBuffer]  
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

