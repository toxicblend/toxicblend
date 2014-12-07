package org.toxicblend.operations.meshgenerator

import org.toxicblend.ToxicblendException
import org.toxicblend.UnitSystem
import org.toxicblend.util.Time.time
import org.toxicblend.util.NumberUtils.{r2d,d2r,inAscendingOrder}
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.typeconverters.Rings2DConverter
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
import toxi.geom.mesh.TriangleMesh
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import org.toxicblend.vecmath.SutherlandHodgemanClipper
import org.toxicblend.vecmath.WeilerAthertonClipper
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.MutableVec2D
import org.toxicblend.vecmath.Polygon2D
import org.toxicblend.vecmath.AABB2D
import org.toxicblend.util.IntNumberUtils.{max,min,abs,sqrt}
import org.toxicblend.util.CyclicTree
import org.toxicblend.vecmath.EarClippingTriangulator

import scala.collection.JavaConversions._

class MeshGeneratorOperation extends CommandProcessorTrait {
  
  private val IDEAL_SIZE = 5000 // totally random number
  
  def processDataPerThread(clockwiseClipPolygon:Polygon2D, useQualityTriangulation:Boolean, aabb:AABB2D, center:Vec2D, parts:Int, delta:Double):TriangleMesh = {
    
    @inline def toTVec3D(v:Vec2D):TVec3D = new TVec3D(v.x.toFloat, v.y.toFloat, 0f)
    
    val triangulator = new EarClippingTriangulator(useQualityTriangulation)
    //println("processDataPerThread: aabb.width/delta=" + aabb.width/delta  + " aabb.height/delta=" +  aabb.height/delta) 
    val reducedClipPolygon = {
      val clipAABB = {
        // make the clip polygon a little bit bigger
        val deltaV = Vec2D(delta*0.1d, delta*0.1)
        aabb.growToContainPoint(aabb.min.sub(deltaV)).growToContainPoint(aabb.max.add(deltaV))
      }
      
      val p = SutherlandHodgemanClipper.clip(clockwiseClipPolygon, clipAABB.toPolygon2D(true), Option(true), Polygon2D.ε)
      if (p.isClockwise) p
      else {
        println("********** reducedClipPolygon was anti-clockwise. was forced to reverse it")
        p.reverse
      } 
    }
    val reducedAABB = reducedClipPolygon.bounds 
    val rvMesh = new TriangleMesh
    
    for (xp <- 0 until parts; yp <-0 until parts) {
      val p2 = Vec2D(aabb.min.x + xp*delta, aabb.min.y + yp*delta)
      val p3 = Vec2D(p2.x+delta, p2.y)
      val p1 = Vec2D(p2.x, p2.y+delta)
      val p0 = Vec2D(p3.x, p1.y)
      
      val cp0 = reducedClipPolygon.containsPoint(p0)
      val cp1 = reducedClipPolygon.containsPoint(p1)
      val cp2 = reducedClipPolygon.containsPoint(p2)
      val cp3 = reducedClipPolygon.containsPoint(p3)
      
      val p = Polygon2D(IndexedSeq(p3, p2, p1, p0))
      val intersects = p.intersects(reducedClipPolygon) 
      //println("p=" + p + " intersects=" + intersects + " contains=" + cp0 + cp1 + cp2 + cp3)
      if (cp0 && cp1 && cp2 && cp3 && !intersects) {
        // p is cleanly inside the clip polygon
        val p03d = toTVec3D(p0)
        val p13d = toTVec3D(p1)
        val p23d = toTVec3D(p2)
        val p33d = toTVec3D(p3)
        // try to make the triangles parallel with the circumference
        if (useQualityTriangulation && { val direction = p1.sub(center); direction.x*direction.y<0}) {  
          rvMesh.addFace(p23d, p03d, p13d)
          rvMesh.addFace(p03d, p23d, p33d)
        } else {
          rvMesh.addFace(p33d, p13d, p23d)
          rvMesh.addFace(p33d, p03d, p13d)
        }
      } else if (intersects) { 
        val clippings = WeilerAthertonClipper.clip(p, reducedClipPolygon, Polygon2D.ε )
        if (clippings.size == 0) {
          println("p=" + p)
          println("reducedClipPolygon=" + reducedClipPolygon)
          println("produced nothing!!!")
        } else if (clippings.size == 1 && clippings(0) == p) {
          // every vertex survived clipping intact
          val p03d = toTVec3D(p0)
          val p13d = toTVec3D(p1)
          val p23d = toTVec3D(p2)
          val p33d = toTVec3D(p3)
          rvMesh.addFace(p23d, p03d, p13d)
          rvMesh.addFace(p03d, p23d, p33d)
        } else clippings.foreach(clipped=>{ 
          if (clipped.size < 3){
            println("ignoring junk polygon. N=" + clipped.size)
          } else if (clipped.size == 3 ) {
            val v = clipped.vertices
            val p03d = toTVec3D(clipped.vertices(0))
            val p13d = toTVec3D(clipped.vertices(1))
            val p23d = toTVec3D(clipped.vertices(2))
            rvMesh.addFace(p03d, p23d, p13d)
          } else if (clipped.size == 4 ) {
            val v = clipped.vertices
            val p03d = toTVec3D(clipped.vertices(0))
            val p13d = toTVec3D(clipped.vertices(1))
            val p23d = toTVec3D(clipped.vertices(2))
            val p33d = toTVec3D(clipped.vertices(3))
            rvMesh.addFace(p03d, p23d, p13d)
            rvMesh.addFace(p03d, p33d, p23d)
          } else {
            val v = clipped.vertices
            val triangles = triangulator.triangulatePolygon(v)
            triangles.foreach(t => {
              rvMesh.addFace(toTVec3D(v(t(0))), toTVec3D(v(t(2))), toTVec3D(v(t(1)))) 
            })
          }
        })
      } else if (false) {
        // These are the squares that fell completely outside the clip polygon.
        // They are added anti-clockwise for debugging purposes 
        val p03d = toTVec3D(p0)
        val p13d = toTVec3D(p1)
        val p23d = toTVec3D(p2)
        val p33d = toTVec3D(p3)
        rvMesh.addFace(p03d, p23d, p13d)
        rvMesh.addFace(p03d, p33d, p23d)
      }
    }
    if (false) {
      // Display the working area of the thread with anti-clockwise triangles 
      val boundV = aabb.toIndexedSeq(true)
      val p03d = toTVec3D(boundV(0))
      val p13d = toTVec3D(boundV(1))
      val p23d = toTVec3D(boundV(2))
      val p33d = toTVec3D(boundV(3))
      rvMesh.addFace(p03d, p23d, p13d)
      rvMesh.addFace(p03d, p33d, p23d)
    }
    rvMesh
  }
  
  /**
   * calculate the value of Z
   */
  def adjustZ(mesh:TriangleMesh, clockwiseClipPolygon:Polygon2D, convexHull:Polygon2D, center:Vec2D, calculator:ZCalculator) = {
    
    val cyclicTree = CyclicTree(convexHull, center)
    mesh.vertices.foreach(tp=>{
      val v = MutableVec2D(tp._1.x, tp._1.y).subSelf(center)
      val heading =  v.heading
      val intersectionO = cyclicTree.getIntersectonPoint(heading,center)
      if (intersectionO.isDefined) {
        val d = v.magnitude/center.distanceTo(intersectionO.get)
        tp._1.z = (calculator.calculateZ(d)*IDEAL_SIZE/2d).toFloat
      } else {
        println("No intersection found:" + heading)
      }
    })
  }
  
  def processData(edges:Polygon2DConverter, center:Option[ReadonlyVec3D], parts:Int, calculator:ZCalculator, useMultiThreading:Boolean, useQualityTriangulation:Boolean) : Mesh3DConverter = {
    
    val (polygon,scale) = {
      val tPolygon = edges.polygons(0)
      val tAabb = tPolygon.getBounds
      val maxDimension = if (tAabb.width > tAabb.height) tAabb.width else tAabb.height
      val scale = IDEAL_SIZE/maxDimension
      val p = Polygon2D(tPolygon.toIndexedSeq.map(v => Vec2D(v.x*scale,v.y*scale)))
      if (p.isClockwise) 
        (p,scale.toFloat) 
      else {
        println("********** ClipPolygon was anti-clockwise. was forced to reverse it")
        (p.reverse,scale.toFloat)  
      }
    }
    val convexHullPolygon = polygon.toConvexHull2(Option(true))
    val convexHullAABB = {
       convexHullPolygon.bounds
    }
    
    val delta = math.max(convexHullAABB.width,convexHullAABB.height).toDouble / parts.toDouble
    val numberOfCores = Runtime.getRuntime().availableProcessors()
    val mtParts = parts/sqrt(numberOfCores,.5d)
    //println("number of cores=" + numberOfCores + " parts = " + parts + " mtParts = " + mtParts)

    val realCenter = if (center.isDefined) {
      val c = center.get
      Vec2D(c.x*scale, c.y*scale)
    } else {
      convexHullPolygon.getCentroid
    }
    
    val resultingMesh = if (useMultiThreading && mtParts >= 2 ) {

      val job = for (xp <- 0 until parts by mtParts;
                     yp <- 0 until parts by mtParts 
                     if (xp * delta < convexHullAABB.width)
                     if (yp * delta < convexHullAABB.height)) yield {
        val x0 = convexHullAABB.min.x+xp*delta
        val y0 = convexHullAABB.min.y+yp*delta
        AABB2D(x0, y0, x0 + mtParts*delta, y0 + mtParts*delta)
      }
      
      //println("subjobs:\n" + job.mkString("\n") + "\nsize=" + job.size + "\n")
      job.par.map(j=>processDataPerThread(polygon,useQualityTriangulation, j,realCenter, parts=mtParts,delta=delta)).seq.foldLeft(new TriangleMesh)((rv,part)=>rv.addMesh(part))
    } else { 
      //val aabb = {
        // make the aabb a little bit bigger to include every pixel
      //  val deltaV = Vec2D(delta*0.1d, delta*0.1)
      //  convexHullAABB.growToContainPoint(convexHullAABB.min.sub(deltaV)).growToContainPoint(convexHullAABB.max.add(deltaV))
      //}
      processDataPerThread(polygon,useQualityTriangulation, convexHullAABB,realCenter,parts=parts,delta=delta)
    }
    
    adjustZ(resultingMesh, polygon, convexHullPolygon, realCenter, calculator)
    
    //mesh.getVertices.map(v => if (v.x.isNaN || v.y.isNaN || v.z.isNaN ) println(v))
    val rv = Mesh3DConverter(resultingMesh.scale(1f/scale), "procedural mesh")
    //println(rv.getVertices.mkString(","))
    if (edges.transforms.size > 0) rv.transform(edges.transforms(0) )
    //println(rv.getVertices.mkString(","))
    rv
  }
    
  def processInput(inMessage:Message, options:OptionConverter) = {
    val traceMsg = "MeshGeneratorOperation"
    
    println(options)
    val useMultiThreading = options.getMultiThreadingProperty(traceMsg,true)
    val useQualityTriangulation = options.getBooleanProperty("qualityTriangulation", false, traceMsg)

    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitIsMetric = options.getUnitSystemProperty(traceMsg)
    val zAlgorithm = options.getStringProperty("zAlgorithm", "CIRCLEARC")
    val (radius1Property,radius2Property) = inAscendingOrder( options.getFloatProperty("radius1", 0f, traceMsg),
                                                              options.getFloatProperty("radius2", 1f, traceMsg) )
    val subdivisions = options.getIntProperty("subdivisions", 2, traceMsg)
    
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
    
    if (edgeModels.size!=1) {
      throw new ToxicblendException("This operation requires one object made out of edges and one optional object containing only one center vertex.")
    }
    
    //println("edge matrix:" + edgeModels(0)._2.get.matrix.toString)
    //println("center matrix:" + centerModels(0)._2.get.matrix.toString)
    
    
    val edgePolygon = time("FindPlanes calculation time: ", {
      def findSequenceOfPolygons( model:(Mesh3DConverter,Option[Matrix4x4Converter]) ) = {
        val segments = model._1.findContinuousLineSegments._2.filter(seq => seq.size>2)
        if (segments.size == 0) System.err.println(traceMsg + ": No edge sequence found in input model.")  
        val pt = Polygon2DConverter.toPolygon2D(segments)
        val name = model._1.name+ " edge input"
        new Polygon2DConverter(pt.map(p => p._1), pt.map(t => t._2), name)
      }
      edgeModels.map(model => findSequenceOfPolygons(model))
    })(0)
    
    if (edgePolygon.polygons.size<1) {
      throw new ToxicblendException("Could not find any connected edge loop in the input.")
    }
    
    val center = if (centerModels.size > 0) Option(centerModels(0)._1.getVertices(0))
                 else None
                 
    time("Building resulting pBModel: ",{
      val returnMessageBuilder = Message.newBuilder
      val calculator = zAlgorithm match {
        case "CIRCLEINTERSECTION" => new IntersectionCalculator(radius1Property,radius2Property)
        case "CIRCLEARC" => new ArcCalculator(radius1Property,radius2Property)
      }
      val parts = subdivisions+1
      returnMessageBuilder.addModels(processData(edgePolygon, center, parts, calculator, useMultiThreading, useQualityTriangulation).toPBModel(None, None))
      returnMessageBuilder
    })
  }
}

