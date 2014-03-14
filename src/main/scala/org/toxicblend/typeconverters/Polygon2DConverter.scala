package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Polygon2D
import toxi.geom.Vec2D
import toxi.geom.Matrix4x4
import toxi.geom.Rect
import org.toxicblend.geometry.ProjectionPlane
import org.toxicblend.geometry.Rings2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.JavaConverters._

/**
 * A toxi.geom.Polygon2D decorator  
 */
class Polygon2DConverter protected (val polygons:ArrayBuffer[Polygon2D],
                                    val projectionPlane:ProjectionPlane.ProjectionPlane, 
                                    val name:String) {
  
  /**
   * Create a packet buffer model from this Rings2D.
   * The result will be a list of 2D points with edges between each point (n, n+1)
   */  
  def toPBModel(finalTransformation:Option[Matrix4x4Converter] ) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder
    modelBuilder.setName(name)
    val helper = new Vertex3DConverterHelper(modelBuilder, projectionPlane, finalTransformation)
    polygons.foreach(poly => {
      helper.addVertex(poly.vertices.get(0))
      poly.vertices.asScala.tail.foreach(v => helper.addVertexAndEdgeToPrevious(v))
      //helper.closeLoop // TODO: do i need to close the loop? Will it even work?
    })

    if (finalTransformation.isDefined) {
      modelBuilder.setWorldOrientation(finalTransformation.get.toPBModel)
    }
    modelBuilder
  }  
}

object Polygon2DConverter {

  /** 
   * Constructs from a Rings2D model
   */
  def apply(rings2d:Rings2D, projectionPlane:ProjectionPlane.ProjectionPlane, name:String):Polygon2DConverter = {
    val polygons = new ArrayBuffer[Polygon2D]
    rings2d.rings.foreach( ring => polygons.append(new Polygon2D(ring.map(i => rings2d.vertices(i)).iterator.asJava)) )
    new Polygon2DConverter(polygons, projectionPlane, name)
  }
  
  /** 
   * Constructs from a Rings2DConverter
   */
  def apply(r2dc:Rings2DConverter):Polygon2DConverter = {
    apply(r2dc.mesh2d, r2dc.projectionPlane, r2dc.name)
  }
}