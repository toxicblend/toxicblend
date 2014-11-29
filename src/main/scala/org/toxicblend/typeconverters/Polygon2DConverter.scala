package org.toxicblend.typeconverters

import org.toxicblend.vecmath.Vec2D
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import org.toxicblend.vecmath.Polygon2D
import toxi.geom.Plane
import toxi.geom.AABB
import toxi.geom.Matrix4x4
import toxi.geom.Matrix3d
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
                          val name:String) {
  assert(polygons!=null)
  polygons.foreach(p=>assert(p!=null))
  assert(transforms!=null)
  transforms.foreach(t=>assert(t!=null))
  assert(name!=null)
  
  /**
   * Create a packet buffer model from this Rings2D.
   * The result will be a list of 2D points with edges between each point (n, n+1)
   */  
  def toPBModel(finalTransformation:Option[Matrix4x4Converter] ) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder
    modelBuilder.setName(name)

    val helper = new Vertex3DHelper(modelBuilder, finalTransformation)
    polygons.zip(transforms).foreach( pt => {
      val firstIndex = {
        val firstVertex = pt._2.applyToSelf(ProjectionPlane.convert(ProjectionPlane.XY_PLANE,pt._1.vertices.head))
        helper.addVertex(firstVertex)
      }
      pt._1.vertices.tail.foreach(v2d => {
        val v3d = pt._2.applyToSelf(ProjectionPlane.convert(ProjectionPlane.XY_PLANE,v2d))
        helper.addVertexAndEdgeToPrevious(v3d)
      })
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
      val p = Polygon2D( ring.map(i => {
        val v = rings2d.vertices(i)
        Vec2D(v.x, v.y)
      }))
      
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
  
  /**
   * Try to calculate the plane normal from a few samples 
   */
  protected def getPlaneQuick(segment:IndexedSeq[ReadonlyVec3D]):(Plane,ReadonlyVec3D) = {
    val aabb = new AABB(segment.head, 0f)
    segment.foreach(s => aabb.growToContainPoint(s))
    
    var vecA = segment(0).sub(aabb)
    var vecB = segment(1).sub(aabb)
    var quality = vecA.cross(vecB).magSquared
    
    val step = math.max(segment.size/15,1).toInt

    //println("Step=" + step + " size=" + segment.size)
    val plane = { (1 until segment.size-1 by step).foreach(i => {
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
      new Plane(aabb, normal)  // normal will be normalized inside Plane constructor
    }
    (plane,vecA)
  }
  
  /**
   * Ported from http://missingbytes.blogspot.com/2012/06/fitting-plane-to-point-cloud.html
   */
  @inline
  protected def findLargestMatrixEntry(m:Matrix3d):Double = {
    var le = math.abs(m.m00)
    le = math.max(le,math.abs(m.m01))
    le = math.max(le,math.abs(m.m02))
    le = math.max(le,math.abs(m.m10))
    le = math.max(le,math.abs(m.m11))
    le = math.max(le,math.abs(m.m12))
    le = math.max(le,math.abs(m.m20))
    le = math.max(le,math.abs(m.m21))
    math.max(le,math.abs(m.m22))
  }
  
  /**
   * Ported from http://missingbytes.blogspot.com/2012/06/fitting-plane-to-point-cloud.html
   * note: This function will perform badly if the largest eigenvalue is complex
   */
  protected def findEigenVectorAssociatedWithLargestEigenValue(m:Matrix3d):ReadonlyVec3D = {
    //pre-condition
    val mc = new Matrix3d(m)
    mc.mul(1.0/findLargestMatrixEntry(m))
   
    mc.mul(mc)
    mc.mul(mc)
    mc.mul(mc)
  
    val v = new Vec3D(1,1,1)
    val lastV = v.copy
    var i=0
    do {
      lastV.set(v)
      mc.transform(v); v.normalize          
      i+=1
    } while (i<100 && v.distanceToSquared(lastV) > 1E-26f )
    //println("Found normal:" + v + " with i=" + i + " distance=" + v.distanceToSquared(lastV))
    v
  }
 
  /**
   * Ported from http://missingbytes.blogspot.com/2012/06/fitting-plane-to-point-cloud.html 
   */
  protected def getLLSQPlane(segment:IndexedSeq[ReadonlyVec3D]):Option[(Plane,ReadonlyVec3D)] = {
    
    val center = segment.foldLeft(new Vec3D)((b,a) => b.addSelf(a)).scaleSelf(1.0f/segment.size)
    var sumxx,sumxy,sumxz,sumyy,sumyz,sumzz = .0

    segment.foreach(p => {
      val dx = (p.x-center.x).toDouble
      val dy = (p.y-center.y).toDouble 
      val dz = (p.z-center.z).toDouble 
      sumxx += dx*dx
      sumxy += dx*dy
      sumxz += dx*dz
      sumyy += dy*dy
      sumyz += dy*dz
      sumzz += dz*dz
    })
    
    val symmetricM = new Matrix3d( sumxx, sumxy, sumxz,
                                   sumxy, sumyy, sumyz,
                                   sumxz, sumyz, sumzz)
    val det=symmetricM.determinant
    if(det==0.0){
      //println("getLLSQPlane: Found NO normal with center = " + center + " det=" + det)
      //println(symmetricM)
      None
    } else {
      //println("getLLSQPlane: Found normal with center = " + center + " det=" + det)
      //println(symmetricM)
      symmetricM.invert
      val destNormal=findEigenVectorAssociatedWithLargestEigenValue(symmetricM)
      
      val forward = {
        val segmentIterator = segment.iterator
        val candidate = new Vec3D
        do {
          candidate.set(segmentIterator.next).subSelf(center)
        } while (segmentIterator.hasNext && candidate.isZeroVector )
        candidate.normalize
        // All vectors in the segment can't be equal to center, can they? 
      }
      //println("getLLSQPlane found normal:" + destNormal + " with center = " + center + " forward=" + forward + " forward.dot(normal)=" + forward.dot(destNormal))
      Option(new Plane(center,destNormal), forward)
    }
  }
  
  /**
   * 
   */
  protected def getPlaneAndMatrix(segment:IndexedSeq[ReadonlyVec3D]):Option[(Plane,Matrix4x4)] = {
    //val (plane, forward) = getPlaneQuick(segment)
    val llsq = getLLSQPlane(segment)
    val (plane, forward) = if (llsq.isDefined) {
       llsq.get
    } else {
      getPlaneQuick(segment)    
    }
    if (plane.normal.isZeroVector) {
      System.err.println("Polygon2DConverter:Normal was zero, ignoring")
      None
    } else {
      //val mt = (new Matrix4x4).identity().rotateAroundAxis(Vec3D.Z_AXIS,math.Pi)
      //println("mt="+ mt)
      val c1 = forward.copy.normalize
      val c2 = plane.normal.cross(c1).normalize
      //println("c1=" + c1)
      //println("c2=" + c2)
      //println("n=" + plane.normal + " c1.dot(c2)=" + c1.dot(c2) + " c1.dot(n)=" + c1.dot(plane.normal))
      val matrix = new Matrix4x4Extension(c1, c2, plane.normal)
      val center =  matrix.applyTo(plane)
      //println("b4" + matrix + " center=" + center)
      matrix.setTranslate(center.scaleSelf(-1f))
      //println("after" + matrix)
      //val matrixI = (new Matrix4x4(matrix)).invert
      
      //println("plane.origin=" + plane.scale(1f) + "transposed=" + center)
      //println("matrix=" + matrix)
      //segment.foreach(s => println("" + s + " => " + matrix.applyTo(s) ))
      //segment.foreach(s => if (!plane.containsPoint(s)) println("" + s + " is NOT part of the plane. Distance =" + plane.getDistanceToPoint(s) ))
      
      Option((plane,matrix))
    }
  }
  
  /**
   * Try to figure out if a set of vertices resides in one plane, then convert that shape into a Polygon2D 
   * together with the transformation matrix that can convert the Polygon2D back into 3D space.
   * Silently ignore the shape if the vertices do not reside on a single plane
   */
  def toPolygon2D(segments:IndexedSeq[IndexedSeq[ReadonlyVec3D]]):IndexedSeq[(Polygon2D,Matrix4x4)] = {
    val rv = segments.par.filter(s => s.size>2).map(segment => {
      getPlaneAndMatrix(segment).map(pm => {
        //println("Segment " + segment.mkString(",") + " has plane:"+ pm._1 + " does that make sense?" )

        val matrix = pm._2
        val mapped3DPoints = {
          val s:IndexedSeq[ReadonlyVec3D] = segment.map(v => matrix.applyTo(v))
          s//s.iterator.asJava
        }
        //println("mapped3DPoints: " + mapped3DPoints.mkString(","))
        val mapped2DPoints:IndexedSeq[Vec2D] = mapped3DPoints.map(v => ProjectionPlane.convert(ProjectionPlane.XY_PLANE,v))
         
        if ( mapped2DPoints.size > 2 ) {
	        val polygon = Polygon2D(mapped2DPoints, Option(true))
          val inverted = matrix.invert
	        assert(inverted!=null)
	        Option(polygon,inverted)
        } else {
          System.err.println("debug me! Could not find any 2d points out of : " + mapped3DPoints)
          None
        }
      })
    }.filter(r => r.isDefined).map(r => r.get))
    rv.filter(r => r.isDefined).map(r => r.get).toIndexedSeq
  }
}