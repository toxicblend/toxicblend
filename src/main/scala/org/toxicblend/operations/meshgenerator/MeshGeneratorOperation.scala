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
import toxi.geom.Vec3D
import toxi.geom.{Vec2D => TVec2D}
import toxi.geom.ReadonlyVec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Polygon2D
import toxi.geom.Line2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.mesh.TriangleMesh
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import org.toxicblend.operations.meshgenerator.vecmath.SutherlandHodgemanClipper
import org.toxicblend.operations.meshgenerator.vecmath.ImmutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.MutableVec2D

class MeshGeneratorOperation extends CommandProcessorTrait {
  
  /**
   * Look at http://paulbourke.net/geometry/circlesphere/
   */
  def calculateZ(point:ReadonlyVec2D, center:ReadonlyVec2D, r:Float) : Vec3D = {
    if (point.x.isNaN || point.y.isNaN) {
      new Vec3D(0,0,0)
    } else {
      val d = center.distanceTo(point)
      if (d != 0) {
        val h = math.sqrt((4d*d*d*r*r-d*d*d*d)/(4d*d*d)).toFloat
        new Vec3D(point.x, point.y, 0)
      } else {
        new Vec3D(point.x, point.y, 0)
      }
    }
  }
  
  def processData(edges:Polygon2DConverter, center:ReadonlyVec3D, subdivisions:Int, radius:Float) : Mesh3DConverter = {
    val polygon = edges.polygons(0)
    val clipEdges = polygon.toIndexedSeq.map(v => new ImmutableVec2D(v.x,v.y))
    
    val aabb = polygon.getBounds
    val deltaX = aabb.width / subdivisions.toFloat
    val deltaY = aabb.height / subdivisions.toFloat
      
    val realCenter3d = if (edges.transforms.size > 0) {
      val c = edges.transforms(0).applyTo(center) // center.copy //
      new Vec3D(c.x, c.y, 0)
    } else new Vec3D(center.x, center.y, 0)
    //val realCenter2d = new Vec2D(realCenter3d.x, realCenter3d.y)
    //val realInverseCenter2d = realCenter2d.scale(-1)
    val clipper = SutherlandHodgemanClipper.singleton
    
    val knownPoints = new HashMap[ReadonlyVec2D, Boolean]
    
    def containsPoint(p:ReadonlyVec2D):Boolean = {
      knownPoints.getOrElse(p, {
        val rv = polygon.containsPoint(p)
        knownPoints.put(p, rv)
        rv })
    }
    
    val mesh = new TriangleMesh

    for (xp <- 0 to subdivisions; yp <-0 to subdivisions) {
      val p2 = new ImmutableVec2D(aabb.x + xp*deltaX, aabb.y + yp*deltaY)
      val p3 = new ImmutableVec2D(p2.x-deltaX, p2.y)
      val p0 = new ImmutableVec2D(p2.x, p2.y-deltaY)
      val p1 = new ImmutableVec2D(p3.x, p0.y)
      /*
      val cp0 = containsPoint(p0)
      val cp1 = containsPoint(p1)
      val cp2 = containsPoint(p2)
      val cp3 = containsPoint(p3)
      
      
      if (cp0 || cp1 || cp2 || cp3) {
        println("p0=" + p0 + " is " + (if (cp0) "inside" else "outside"))
        println("p1=" + p1 + " is " + (if (cp1) "inside" else "outside"))
        println("p2=" + p2 + " is " + (if (cp2) "inside" else "outside"))
        println("p3=" + p3 + " is " + (if (cp3) "inside" else "outside"))
      }
      
      if (cp0 && cp1 && cp2 && cp3) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        mesh.addFace(p03d, p23d, p13d)
        mesh.addFace(p13d, p23d, p33d)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p0, p1, p2).iterator).isClockwise())
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p1, p3, p2).iterator).isClockwise()) 
      } else if (cp0 && cp1 && cp2 ) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p0, p1, p2).iterator).isClockwise())
        mesh.addFace(p03d, p23d, p13d)
      } else if (cp1 && cp3 && cp2 ) {
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        mesh.addFace(p13d, p23d, p33d)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p1, p3, p2).iterator).isClockwise())
      } else if (cp0 && cp1 && cp3 ) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        mesh.addFace(p03d, p33d, p13d)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p0, p1, p3).iterator).isClockwise())
      } else if (cp0 && cp3 && cp2 ) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        mesh.addFace(p03d, p23d, p33d)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p0, p3, p2).iterator).isClockwise())
      } else 
      * 
      */
      //if (cp0 || cp1 || cp2 || cp3) { 
         
        //println("new Polygon2D is clockwise: " + new Polygon2D(p.iterator).isClockwise())
        val p = clipper.clipPolygon(ArrayBuffer(p3, p2, p0, p1), clipEdges)
        //val p = ArrayBuffer(p3, p1, p0, p2)
        //println("size=" + p.size + " : " + p.mkString(","))
        /*if (p.size <= 4) {
          println("size=" + p.size + " : " + p.mkString(","))
          println("new 3Polygon2D is clockwise: " + new Polygon2D(p.iterator).isClockwise())
          mesh.addFace(calculateZ(p.get(0), realCenter2d, radius), calculateZ(p.get(1), realCenter2d, radius), calculateZ(p.get(2), realCenter2d, radius) )
        } else 
        */
        if (p.size >= 3) {
          println("size=" + p.size + " : " + p.mkString(","))
          //println("b4")
          //println("new "+ p.size + " Polygon2D is clockwise: " + new Polygon2D(p.iterator).isClockwise())
          mesh.getVertices.map(v => if (v.x.isNaN || v.y.isNaN || v.z.isNaN ) println(v))
          mesh.getVertices.map(v => if (v.x.isInfinite || v.y.isInfinite || v.z.isInfinite ) println(v))
         
          val centroid = p.foldLeft(new TVec2D)((x,s) => x.addSelf(s.x.toFloat, s.y.toFloat)).scaleSelf(1f/(p.size.toFloat))
          new Polygon2D(p.map(v=>new TVec2D(v.x.toFloat, v.y.toFloat)).iterator).toMesh(mesh, centroid, 0f)
          
          println("centroid:" + centroid)
          mesh.getVertices.map(v => if (v.x.isNaN || v.y.isNaN || v.z.isNaN ) println(v))
        }
      //}
    }
    //println(mesh.vertices.mkString(","))
    //println("mesh:")
    
    mesh.getVertices.map(v => if (v.x.isNaN || v.y.isNaN || v.z.isNaN ) println(v))
    val rv = Mesh3DConverter(mesh.faceOutwards, "procedural mesh")
    println(rv.getVertices.mkString(","))
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

