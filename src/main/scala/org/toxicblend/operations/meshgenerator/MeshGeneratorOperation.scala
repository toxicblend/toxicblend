package org.toxicblend.operations.meshgenerator

import org.toxicblend.ToxicblendException
import org.toxicblend.UnitSystem
import org.toxicblend.util.Time.time
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.geometry.ProjectionPlane.YZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XZ_PLANE
import org.toxicblend.geometry.ProjectionPlane.XY_PLANE
import org.toxicblend.typeconverters.Rings2DConverter
import org.toxicblend.util.Regex
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Polygon2DConverter
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.operations.boostmedianaxis.MedianAxisJni.simplify3D
import toxi.geom.{Vec3D => TVec3D}
import toxi.geom.{Vec2D => TVec2D}
import toxi.geom.ReadonlyVec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.{Polygon2D=>TPolygon2D}
import toxi.geom.Line2D
import scala.collection.mutable.ArrayBuffer
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.mesh.TriangleMesh
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import org.toxicblend.operations.meshgenerator.vecmath.SutherlandHodgemanClipper
import org.toxicblend.operations.meshgenerator.vecmath.WeilerAthertonClipper
import org.toxicblend.operations.meshgenerator.vecmath.Vec2D
import org.toxicblend.operations.meshgenerator.vecmath.MutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.Polygon2D
import org.toxicblend.operations.meshgenerator.vecmath.AABB2D

import scala.collection.JavaConversions._

class MeshGeneratorOperation extends CommandProcessorTrait {
  
  /**
   * Ported from http://paulbourke.net/geometry/circlesphere/
   * /
  def calculateZ(point:ReadonlyVec2D, center:ReadonlyVec2D, r:Float) : TVec3D = {
    if (point.x.isNaN || point.y.isNaN) {
      new TVec3D(0,0,0)
    } else {
      val d = center.distanceTo(point)
      if (d != 0) {
        val h = math.sqrt((4d*d*d*r*r-d*d*d*d)/(4d*d*d)).toFloat
        new TVec3D(point.x, point.y, 0)
      } else {
        new TVec3D(point.x, point.y, 0)
      }
    }
  }
  */
  
  def processDataPerThread(clockwisePolygon:Polygon2D, aabb:AABB2D, delta:Double):TriangleMesh = {
    
    def calculateZ(v:Vec2D):TVec3D = new TVec3D(v.x.toFloat, v.y.toFloat, 0)
    println("clockwisePolygon.isSelfIntersecting=" + clockwisePolygon.isSelfIntersecting)
    println("clockwisePolygon.isClockwise=" + clockwisePolygon.isClockwise)

    val reducedClipPolygon = {
      val p = SutherlandHodgemanClipper.clip(clockwisePolygon, aabb.toPolygon2D(true), 0.0001)
      if (p.isClockwise) p
      else {
        println("********** reducedClipPolygon was anti-clockwise. was forced to reverse it")
        new Polygon2D(p.vertices.reverse)
      }
    }
    val reducedAABB = reducedClipPolygon.bounds
    
    println("aabb=" + aabb)
    println("aabb.width=" + aabb.width)
    println("aabb.height=" + aabb.height)
    
    val rvMesh = new TriangleMesh
    val subdivisionsX = (0.5d+(aabb.width/delta)).toInt
    val subdivisionsY = (0.5d+(aabb.height/delta)).toInt
    
    println("subdivisionsX=" + subdivisionsX)
    println("subdivisionsY=" + subdivisionsY)
    
    println("reducedAABB=" + reducedAABB)
    println("reducedClipPolygon=" + reducedClipPolygon.vertices.mkString(","))
    println("reducedClipPolygon.isClockwise=" + reducedClipPolygon.isClockwise)
    println("reducedClipPolygon.isSelfIntersecting=" + reducedClipPolygon.isSelfIntersecting)
    println("reducedAABB.center=" + reducedAABB.x + " " + reducedAABB.y)
    println("reducedClipPolygon.minCenterDistanceSquared=" + reducedClipPolygon.minCenterDistanceSquared)

    println("reducedAABB=" + reducedAABB)
    println("reducedAABB.width=" + reducedAABB.width)
    println("reducedAABB.height=" + reducedAABB.height)
    
    println("delta=" + delta)
    
    for (xp <- 0 to subdivisionsX; yp <-0 to subdivisionsY) yield {
      val p2 = Vec2D(aabb.min.x + xp*delta, aabb.min.y + yp*delta)
      val p3 = Vec2D(p2.x+delta, p2.y)
      val p1 = Vec2D(p2.x, p2.y+delta)
      val p0 = Vec2D(p3.x, p1.y)
      
      val cp0 = reducedClipPolygon.containsPoint(p0)
      val cp1 = reducedClipPolygon.containsPoint(p1)
      val cp2 = reducedClipPolygon.containsPoint(p2)
      val cp3 = reducedClipPolygon.containsPoint(p3)
      
      if (false) if (cp0 || cp1 || cp2 || cp3) {
        println("p0=" + p0 + " is " + (if (cp0) "inside" else "outside"))
        println("p1=" + p1 + " is " + (if (cp1) "inside" else "outside"))
        println("p2=" + p2 + " is " + (if (cp2) "inside" else "outside"))
        println("p3=" + p3 + " is " + (if (cp3) "inside" else "outside"))
      }
      
      if (cp0 && cp1 && cp2 && cp3) {
        val p03d = calculateZ(p0)
        val p13d = calculateZ(p1)
        val p23d = calculateZ(p2)
        val p33d = calculateZ(p3)
        rvMesh.addFace(p23d, p13d, p03d)
        rvMesh.addFace(p03d, p33d, p23d)
        //val p = new Polygon2D(Array(p0, p1, p2, p3))
        //println("new Polygon p0, p1, p3, p2 is clockwise: " + p.isClockwise)
        //println("new Polygon p0, p1, p3, p2 is isSelfIntersecting: " + p.isSelfIntersecting)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p1, p3, p2).iterator).isClockwise()) 
      } /*else if (cp0 && cp1 && cp3 ) {
        val p03d = calculateZ(p0)
        val p13d = calculateZ(p1)
        val p33d = calculateZ(p3)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p0, p1, p2).iterator).isClockwise())
        rvMesh.addFace(p03d, p13d, p33d)
      } else if (cp1 && cp3 && cp2 ) {
        val p13d = calculateZ(p1)
        val p23d = calculateZ(p2)
        val p33d = calculateZ(p3)
        rvMesh.addFace(p13d, p33d, p23d)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p1, p3, p2).iterator).isClockwise())
      } else if (cp3 && cp2 && cp0 ) {
        val p03d = calculateZ(p0)
        val p23d = calculateZ(p2)
        val p33d = calculateZ(p3)
        rvMesh.addFace(p03d, p33d, p23d)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p0, p1, p3).iterator).isClockwise())
      } else if (cp2 && cp0 && cp1 ) {
        val p23d = calculateZ(p2)
        val p03d = calculateZ(p0)
        val p13d = calculateZ(p1)
        rvMesh.addFace(p23d, p03d, p13d)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p0, p3, p2).iterator).isClockwise())
      } */ else if (cp0 || cp1 || cp2 || cp3) { 
        
        val p = new Polygon2D(IndexedSeq(p0, p1, p2, p3))
        //println("p is clockwise: " + p.isClockwise)
        //println("p is selfintersecting: " + p.isSelfIntersecting)
            
        //println("new Polygon2D is clockwise: " + new Polygon2D(p.iterator).isClockwise())
        WeilerAthertonClipper.clip(p, reducedClipPolygon, Polygon2D.ε ).foreach(clipped=>{ 
          if(false) {
            
            println("p.size = " + p.size )
            println("p.bounds = " + p.bounds )
            println("p.center = " + p.bounds.x + "," + p.bounds.y  )
            println("p = " + p.vertices.mkString(","))
            println("p=" + p)
            println("reducedClipPolygon=" + reducedClipPolygon)
          } 
          if (clipped.size >= 3) {
            if(false) {
              println("clipped.isClockwise = " + clipped.isClockwise )
              println("clipped.size = " + clipped.size )
              println("clipped.bounds = " + clipped.bounds )
              println("clipped.center = " + clipped.bounds.x + "," + clipped.bounds.y  )
              println("clipped = " + clipped.vertices.mkString(","))
            }
             
            //rvMesh.getVertices.map(v => if (v.x.isNaN || v.y.isNaN || v.z.isNaN ) println(v))
            //rvMesh.getVertices.map(v => if (v.x.isInfinite || v.y.isInfinite || v.z.isInfinite ) println(v))
           
            val centroid = {
              val c = clipped.getCentroid
              new TVec2D(c.x.toFloat, c.y.toFloat)
            }
            //println("clipped.centroid = " + centroid )
            new TPolygon2D(clipped.vertices.map(v=>new TVec2D(v.x.toFloat, v.y.toFloat)).iterator).toMesh(rvMesh, centroid, 0f)
            
            //println("centroid:" + centroid)
            //rvMesh.getVertices.map(v => if (v.x.isNaN || v.y.isNaN || v.z.isNaN ) println(v))
          }
        })
      }
    }
    //println(mesh.vertices.mkString(","))
    //println("mesh:")
    rvMesh
  }
  
  def processData(edges:Polygon2DConverter, center:ReadonlyVec3D, subdivisions:Int, radius:Float) : Mesh3DConverter = {
    
    val (polygon,scale) = {
      val polygon = if (edges.polygons(0).isClockwise) edges.polygons(0) else edges.polygons(0).flipVertexOrder
      val aabb = polygon.getBounds
      val maxDimension = if (aabb.width > aabb.height) aabb.width else aabb.height
      val scale = 500d/maxDimension
      (Polygon2D(polygon.toIndexedSeq.map(v => Vec2D(v.x*scale,v.y*scale))),scale)
    }
    
    val aabb = polygon.bounds
    val delta = {
      if (aabb.width > aabb.height) aabb.width / subdivisions.toDouble
      else aabb.height / subdivisions.toDouble
    }
    
    val realCenter3d = if (edges.transforms.size > 0) {
      val c = edges.transforms(0).applyTo(center) // center.copy //
      new TVec3D(c.x, c.y, 0)
    } else new TVec3D(center.x, center.y, 0)
    //val realCenter2d = new Vec2D(realCenter3d.x, realCenter3d.y)
    //val realInverseCenter2d = realCenter2d.scale(-1)
       
    val resultingMesh = processDataPerThread(polygon,aabb,delta)
    
    //mesh.getVertices.map(v => if (v.x.isNaN || v.y.isNaN || v.z.isNaN ) println(v))
    val rv = Mesh3DConverter(resultingMesh.scale(1f/scale.toFloat), "procedural mesh")
    //println(rv.getVertices.mkString(","))
    if (edges.transforms.size > 0) rv.transform(edges.transforms(0) )
    //println(rv.getVertices.mkString(","))
    rv
  }
    
  def processInput(inMessage:Message, options:OptionConverter) = {
    val traceMsg = "MeshGeneratorOperation"
    
    println(options)
    val useMultiThreading = options.getMultiThreadingProperty(traceMsg,true)
    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitIsMetric = options.getUnitSystemProperty(traceMsg)
    val radius = options.getFloatProperty("radius", 0.1f, traceMsg)*unitScale/1000f // convert from meter to mm
    val subdivisions = options.getIntProperty("subdivisions", 1, traceMsg)
    
    // Convert model vertices to world coordinates so that the radius unit makes sense
    val edgeModels = inMessage.getModelsList.filter(m => m.getVerticesCount>1).map(inModel => {
      (Mesh3DConverter(inModel,true), // Unit is now [meter]
      if (inModel.hasWorldOrientation) {
        Option(Matrix4x4Converter(inModel.getWorldOrientation))
      } else {
        None
      })
    })
    
    val centerModels = inMessage.getModelsList.filter(m => m.getVerticesCount==1).map(inModel => {
      (Mesh3DConverter(inModel,true), // Unit is now [meter]
      if (inModel.hasWorldOrientation) {
        Option(Matrix4x4Converter(inModel.getWorldOrientation))
      } else {
        None
      })
    })
    
    if (edgeModels.size!=1 || centerModels.size !=1) {
      throw new ToxicblendException("This operation requires one object made out of edges and one object containing only one center vertex.")
    }
    
    //println("edge matrix:" + edgeModels(0)._2.get.matrix.toString)
    //println("center matrix:" + centerModels(0)._2.get.matrix.toString)
    
    
    val edgePolygons = time("FindPlanes calculation time: ", {
      def findSequenceOfPolygons( model:(Mesh3DConverter,Option[Matrix4x4Converter]) ) = {
        val segments = model._1.findContinuousLineSegments._2.filter(seq => seq.size>2)
        if (segments.size == 0) System.err.println(traceMsg + ": No edge sequence found in input model.")  
        val pt = Polygon2DConverter.toPolygon2D(segments)
        val name = model._1.name+ " edge input"
        new Polygon2DConverter(pt.map(p => p._1), pt.map(t => t._2), name)
      }
      edgeModels.map(model => findSequenceOfPolygons(model))
    })
                    
    time("Building resulting pBModel: ",{
      val returnMessageBuilder = Message.newBuilder
      returnMessageBuilder.addModels(processData(edgePolygons(0), centerModels(0)._1.getVertices(0), subdivisions, radius).toPBModel(None, None))
      returnMessageBuilder
    })
  }
}

