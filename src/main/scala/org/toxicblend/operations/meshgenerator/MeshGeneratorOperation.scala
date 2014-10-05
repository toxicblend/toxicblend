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
import toxi.geom.Vec2D
import toxi.geom.ReadonlyVec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Polygon2D
import toxi.geom.Line2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.mesh.TriangleMesh

class MeshGeneratorOperation extends CommandProcessorTrait {
  
  /**
   * Look at http://paulbourke.net/geometry/circlesphere/
   */
  def calculateZ(point:ReadonlyVec2D, center:ReadonlyVec2D, r:Float) : Vec3D = {
    val d = center.distanceTo(point)
    val h = math.sqrt((4d*d*d*r*r-d*d*d*d)/(4d*d*d)).toFloat
    new Vec3D(point.x, point.y, 0)
  }
  
  def processData(edges:Polygon2DConverter, center:ReadonlyVec3D, radius:Float) : Mesh3DConverter = {
    val polygon = edges.polygons(0)
    val aabb = polygon.getBounds
    val deltaX = aabb.width / 50.0f
    val deltaY = aabb.height / 50.0f
      
    val realCenter3d = if (edges.transforms.size > 0) {
      val c = edges.transforms(0).applyTo(center) // center.copy //
      new Vec3D(c.x, c.y, 0)
    } else {
      new Vec3D(center.x, center.y, 0)
    }
    val realCenter2d = new Vec2D(realCenter3d.x, realCenter3d.y)
    val realInverseCenter2d = realCenter2d.scale(-1)
    val clipper = new SutherlandHodgemanClipper
    
    val points = polygon.toIndexedSeq.map( p => {
      val v = p.sub(realCenter2d)
      new Payload(v.heading, v.magnitude, p)
    })
    val tree = CyclicTree(points)
    
    def containsPoint(p:ReadonlyVec2D):Boolean = {
      val v = p.sub(realCenter2d)
      val candidate = tree.searchIntervalWithLimits(v.heading)
      if (candidate.isDefined) {
        val mag = v.magnitude 
        if (mag < candidate.get._1.distance && mag < candidate.get._2.distance ) true
        else false
      } else {
        polygon.containsPoint(p)
      }
    }
    
    val closedLoop = polygon.toIndexedSeq :+ polygon.get(0) // close the loop
    
    val mesh = new TriangleMesh
    if (false) 
      closedLoop.sliding(2,1).foreach(p => {
        mesh.addFace(new Vec3D(p(0).x,p(0).y, 0), new Vec3D(p(1).x,p(1).y, 0), realCenter3d)
      })
    for (xp <- 0 until 100; yp <-0 until 100) {
      val insidePoints = new ArrayBuffer[Vec2D]
      val outsidePoints = new ArrayBuffer[Vec2D]
      val p2 = new Vec2D(aabb.x + xp*deltaX, aabb.y + yp*deltaY)
      val p3 = new Vec2D(p2.x-deltaX, p2.y)
      val p0 = new Vec2D(p2.x, p2.y-deltaY)
      val p1 = new Vec2D(p3.x, p0.y)
      val cp0 = containsPoint(p0)
      val cp1 = containsPoint(p1)
      val cp2 = containsPoint(p2)
      val cp3 = containsPoint(p3)
      
      if (cp0 && cp1 && cp2 && cp3) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        mesh.addFace(p03d, p13d, p23d)
        mesh.addFace(p13d, p33d, p23d)
      } else if (cp0 && cp1 && cp2 ) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        mesh.addFace(p03d, p13d, p23d)
      } else if (cp1 && cp3 && cp2 ) {
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        mesh.addFace(p13d, p33d, p23d)
      } else if (cp0 && cp1 && cp3 ) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p13d = calculateZ(p1, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        mesh.addFace(p03d, p13d, p33d)
      } else if (cp0 && cp3 && cp2 ) {
        val p03d = calculateZ(p0, realCenter2d, radius)
        val p33d = calculateZ(p3, realCenter2d, radius)
        val p23d = calculateZ(p2, realCenter2d, radius)
        mesh.addFace(p03d, p33d, p23d)
      } else if (cp0 || cp1 || cp2 || cp3) {
      
        var p = new Polygon2D
        p.add(p0)
        p.add(p1)
        p.add(p2)
        p.add(p3)
        // move to center
        p.foreach(v => v.subSelf(realCenter2d))
        if (!cp0) {
          val edge = tree.searchIntervalWithLimits(p0.heading)
          p = clipper.clipPolygon(p, new Line2D(edge.get._1.pos.sub(realCenter2d),edge.get._2.pos.sub(realCenter2d)))
        }
        if (!cp1) {
          val edge = tree.searchIntervalWithLimits(p1.heading)
          p = clipper.clipPolygon(p, new Line2D(edge.get._1.pos.sub(realCenter2d),edge.get._2.pos.sub(realCenter2d)))
        }
        if (!cp2) {
          val edge = tree.searchIntervalWithLimits(p2.heading)
          p = clipper.clipPolygon(p, new Line2D(edge.get._1.pos.sub(realCenter2d),edge.get._2.pos.sub(realCenter2d)))
        }
        if (!cp3) {
          val edge = tree.searchIntervalWithLimits(p3.heading)
          p = clipper.clipPolygon(p, new Line2D(edge.get._1.pos.sub(realCenter2d),edge.get._2.pos.sub(realCenter2d)))
        }
        // move back
        p.foreach(v => v.addSelf(realCenter2d))
        if (p.size == 3) {
          mesh.addFace(calculateZ(p.get(0), realCenter2d, radius), calculateZ(p.get(1), realCenter2d, radius), calculateZ(p.get(2), realCenter2d, radius) )
        } else if (p.size > 3) {
          println("size=" + p.size + " : " + p.mkString(","))
          p.toMesh(mesh)
        }
      }
    }
      
    val rv = Mesh3DConverter(mesh, "procedural mesh")
    if (edges.transforms.size > 0) rv.transform(edges.transforms(0) )
    rv
  }
  
  def processInput(inMessage:Message, options:OptionConverter) = {
    val traceMsg = "Offset2dShapeOperation"
        
    val useMultiThreading = options.getMultiThreadingProperty(traceMsg,true)
    
    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitIsMetric = options.getUnitSystemProperty(traceMsg)
    val radius = options.getFloatProperty("radius", 0.1f, traceMsg)*unitScale/1000f // convert from meter to mm
     
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
      returnMessageBuilder.addModels(processData(edgePolygons(0), centerModels(0)._1.getVertices(0), radius).toPBModel(None, None))
      returnMessageBuilder
    })
  }
}

