package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Polygon2D
import toxi.geom.Plane
import toxi.geom.AABB
import toxi.geom.Vec2D
import toxi.geom.Matrix4x4
import toxi.geom.Rect
import org.toxicblend.geometry.ProjectionPlane
import org.toxicblend.geometry.Matrix4x4Extension
import org.toxicblend.geometry.Rings2D
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._

/**
 * A toxi.geom.Polygon2D decorator  
 */
class Polygon2DConverter (val polygons:Seq[Polygon2D],
                          val transforms:Seq[Matrix4x4],
                          /*val projectionPlane:Option[ProjectionPlane.ProjectionPlane], */
                          val name:String) {
  
  /**
   * Create a packet buffer model from this Rings2D.
   * The result will be a list of 2D points with edges between each point (n, n+1)
   */  
  def toPBModel(finalTransformation:Option[Matrix4x4Converter] ) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder
    modelBuilder.setName(name)
    //val helper = if (projectionPlane.isDefined){
    //  new Vertex3DHelper(modelBuilder, /*ProjectionPlane.convert(projectionPlane.get, _),*/ finalTransformation)
    //} else {
      // TODO
    //  new Vertex3DHelper(modelBuilder, /*ProjectionPlane.convert(projectionPlane.get, _),*/ finalTransformation)
    //}
    val helper = new Vertex3DHelper(modelBuilder, /*ProjectionPlane.convert(projectionPlane.get, _),*/ finalTransformation)
    polygons.zip(transforms).foreach( pt => {
      val firstIndex = helper.addVertex(pt._2.applyToSelf(ProjectionPlane.convert(ProjectionPlane.XY_PLANE,pt._1.vertices.get(0))))
      pt._1.vertices.asScala.tail.foreach(v => helper.addVertexAndEdgeToPrevious(pt._2.applyToSelf(ProjectionPlane.convert(ProjectionPlane.XY_PLANE,v))))
      helper.closeLoop(firstIndex)
    })

    if (finalTransformation.isDefined) {
      modelBuilder.setWorldOrientation(finalTransformation.get.toPBModel)
    }
    //println("Polygon2DConverter pbmodel:" + modelBuilder.getFacesList())
    modelBuilder
  }
  
  override def toString = {
    polygons.toString
  }
}

object Polygon2DConverter {

  /** 
   * Constructs from a Rings2D model
   */
  def apply(rings2d:Rings2D, projectionPlane:ProjectionPlane.ProjectionPlane, name:String):Polygon2DConverter = {
    val polygons = new ListBuffer[Polygon2D]
    rings2d.rings.foreach( ring => {
      val p = new Polygon2D(ring.map(i => rings2d.vertices(i)).iterator.asJava)
      polygons.append(p) 
    })
    new Polygon2DConverter(polygons, new Array[Matrix4x4](0), /*Option(projectionPlane),*/ name)
  }
  
  /** 
   * Constructs from a Rings2DConverter
   */
  def apply(r2dc:Rings2DConverter):Polygon2DConverter = {
    apply(r2dc.mesh2d, r2dc.projectionPlane, r2dc.name)
  }
  
  protected def getPlane(segment:IndexedSeq[ReadonlyVec3D]):Option[(Plane,Matrix4x4)] ={

    val aabb = new AABB(segment.head,0f)
    segment.foreach(s => aabb.growToContainPoint(s))
    
    var vecA = segment(0).sub(aabb)
    var vecB = segment(1).sub(aabb)
    var quality = vecA.cross(vecB).magSquared
    
    val step = {
      // Only sample at most 15 vertices to figure out the normal 
      val s = segment.size/15
      if (s <= 0) 1 else s
    }
    //println("Step=" + step + " size=" + segment.size)
    val plane = { (2 until segment.size-1 by step).foreach(i => {
        val q1=segment(i).sub(aabb).crossSelf(vecB).magSquared
        if (q1 > quality) {
          quality = q1
          vecA = segment(i).sub(aabb)
        }
        val q2=segment(i+1).sub(aabb).crossSelf(vecA).magSquared
        if (q2 > quality) {
          quality = q2
          vecB = segment(i+1).sub(aabb)
        }
      })
      var normal = vecA.cross(vecB)
      new Plane(aabb,normal)  // normal will be normalized inside Plane constructor
    }
    
    if (plane.normal.isZeroVector) {
      println("Normal was zero, ignoring")
      None
    } else {
      //val mt = (new Matrix4x4).identity().rotateAroundAxis(Vec3D.Z_AXIS,math.Pi)
      //println("mt="+ mt)
      val c1 = (new Vec3D(vecA)).normalize
      val c2 = plane.normal.cross(c1).normalize
      //println("c1=" + c1)
      //println("c2=" + c2)
      //println("n=" + plane.normal + " c1.dot(c2)=" + c1.dot(c2) + " c1.dot(n)=" + c1.dot(plane.normal))
      val matrix = new Matrix4x4Extension(c1,c2,plane.normal)
      val center =  matrix.applyTo(plane)
      //println("b4" + matrix + " center=" + center)
      matrix.setTranslate(center.scaleSelf(-1f))
      //println("after" + matrix)
      //val matrixI = (new Matrix4x4(matrix)).invert
      
      //println("plane.origin=" + plane.scale(1f) + "transposed=" + center)
      //println("matrix=" + matrix)
      //segment.foreach(s => println("" + s + " => " + matrix.applyTo(s) ))
      //segment.foreach(s => println("" + s + " => " + plane.containsPoint(s)))
      Option((plane,matrix))
    }
  }
  
  /**
   * Try to figure out if a set of vertices resides in one plane, then convert that shape into a Polygon2D 
   * together with the transformation matrix that can convert the Polygon2D back into 3D space.
   * Silently ignore the shape if the vertices do not reside on a single plane
   */
  def toPolygon2D(segments:IndexedSeq[IndexedSeq[ReadonlyVec3D]]):(IndexedSeq[Polygon2D],IndexedSeq[Matrix4x4]) = {
    val polygonRv = new ArrayBuffer[Polygon2D]
    val matrixRv = new ArrayBuffer[Matrix4x4]
    segments.filter(s => s.size>2).par.foreach(segment => {
      getPlane(segment).foreach(pm => {
        //println("Segment " + segment.mkString(",") + " has plane:"+ pm._1 + " does that make sense?" )

        val matrix = pm._2
        val mapped3DPoints = {
          val s:IndexedSeq[ReadonlyVec3D] = segment.map(v => matrix.applyTo(v))
          s//s.iterator.asJava
        }
        //println("mapped3DPoints: " + mapped3DPoints.mkString(","))
        val mapped2DPoints =  {
          val m2d:IndexedSeq[ReadonlyVec2D] = mapped3DPoints.map(v => ProjectionPlane.convert(ProjectionPlane.XY_PLANE,v))
          m2d.iterator.asJava
        }
        val polygon = new Polygon2D(mapped2DPoints)
        
        //println("polygon: " + polygon)
        if (!polygon.isClockwise) {
          polygon.flipVertexOrder
        }
        polygonRv.append(polygon)
        matrixRv.append(matrix.invert())
      })
    })
    (polygonRv,matrixRv)
  }
}