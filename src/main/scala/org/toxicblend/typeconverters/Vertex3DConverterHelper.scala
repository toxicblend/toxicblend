package org.toxicblend.typeconverters

import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Vec2D
import toxi.geom.Matrix4x4
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import org.toxicblend.geometry.ProjectionPlane


protected[typeconverters] class Vertex3DConverterHelper(val modelBuilder:Model.Builder, 
                                                        val projectionPlane:ProjectionPlane.ProjectionPlane, 
                                                        val finalTransformation:Option[Matrix4x4Converter]) {
  protected var vertexIndex = 0
  
  val inverseFinalTransformation = 
    if (finalTransformation.isDefined) 
      Option({
        val m=new Matrix4x4(finalTransformation.get.matrix)
        m.invert
      })
    else 
      None
  
  def addVertex(vertex:ReadonlyVec2D) = {
    val rv = vertexIndex
    val pbvertex = org.toxicblend.protobuf.ToxicBlendProtos.Vertex.newBuilder()
    pbvertex.setId(vertexIndex)
    val vertex3d = ProjectionPlane.convert(projectionPlane,vertex)
    if (inverseFinalTransformation.isDefined) {
      inverseFinalTransformation.get.applyToSelf(vertex3d)
    }
    pbvertex.setX(vertex3d.x)
    pbvertex.setY(vertex3d.y)
    pbvertex.setZ(vertex3d.z)
    modelBuilder.addVertices(pbvertex)
    vertexIndex += 1
    rv
  }
  
  def addVertexAndEdgeToPrevious(vertex:ReadonlyVec2D) = {
    val rv = addVertex(vertex)
    if ( rv > 0) {
      addFace(rv-1, rv)
    }
    rv
  }
  
  def addFace(face:IndexedSeq[Int]) = {
    val faceBuilder = Face.newBuilder()
    face.foreach(f => faceBuilder.addVertices(f))
    modelBuilder.addFaces(faceBuilder)
  }
  
  /**
   * convenience operator to add a single edge
   */
  @inline
  def addFace(edgeVertex1:Int, edgeVertex2:Int) = {
    val faceBuilder = Face.newBuilder()
    faceBuilder.addVertices(edgeVertex1)
    faceBuilder.addVertices(edgeVertex2)
    modelBuilder.addFaces(faceBuilder)
  }
  
  def closeLoop(firstVertexIndex:Int):Unit = {
    if (vertexIndex > 1 && firstVertexIndex != (vertexIndex-1)) {
      addFace(vertexIndex-1,firstVertexIndex)
      //println("Closing vertex:"+ (vertexIndex-1) + " to:" + firstVertexIndex )
    }
  }
  
  def closeLoop():Unit = closeLoop(0)
}