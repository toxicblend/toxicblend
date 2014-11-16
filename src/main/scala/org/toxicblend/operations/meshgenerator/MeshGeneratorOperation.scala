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
import org.toxicblend.vecmath.CyclicTree
import org.toxicblend.vecmath.EarClipper

import scala.collection.JavaConversions._

class MeshGeneratorOperation extends CommandProcessorTrait {
  
  private val IDEAL_SIZE = 5000 // totally random number
  
  def checkForCorruption1(i:Iterator[toxi.geom.mesh.Vertex]) :Boolean = {
    i.foreach( v=> if (v.x.isNaN || v.y.isNaN || v.z.isNaN || v.x.isInfinite || v.y.isInfinite || v.z.isInfinite ) return true)
    false
  }
  
  def checkForCorruption2(i:Iterator[Vec2D]) :Boolean = {
    i.foreach(v=> if (v.x.isNaN || v.y.isNaN || v.x.isInfinite || v.y.isInfinite ) return true)
    false
  }
  
  def processDataPerThread(clockwiseClipPolygon:Polygon2D, aabb:AABB2D, center:Vec2D, delta:Double):TriangleMesh = {
    
    @inline def toTVec3D(v:Vec2D):TVec3D = new TVec3D(v.x.toFloat, v.y.toFloat, 0f)
    
    val triangulator = new EarClipper
    
    //println("clockwiseClipPolygon.isSelfIntersecting=" + clockwiseClipPolygon.isSelfIntersecting)
    //println("clockwiseClipPolygon.isClockwise=" + clockwiseClipPolygon.isClockwise)
    println("processDataPerThread: aabb.width/delta=" + aabb.width/delta  + " aabb.height/delta=" +  aabb.height/delta) 
    val reducedClipPolygon = {
      val p = SutherlandHodgemanClipper.clip(clockwiseClipPolygon, aabb.toPolygon2D(true), Polygon2D.ε)
      if (p.isClockwise) p
      else {
        println("********** reducedClipPolygon was anti-clockwise. was forced to reverse it")
        p.reverse
      }
    }
    //assert(reducedClipPolygon.isClockwise)
    val reducedAABB = reducedClipPolygon.bounds
    
    if (false) {
      println("aabb=" + aabb)
      println("aabb.width=" + aabb.width)
      println("aabb.height=" + aabb.height)
      println("delta=" + delta)
      println("reducedAABB=" + reducedAABB)
      println("reducedAABB.width=" + reducedAABB.width)
      println("reducedAABB.height=" + reducedAABB.height)
    }
    
    val rvMesh = new TriangleMesh
    val subdivisionsX = math.round((0.5d+(aabb.width/delta)).toFloat)
    val subdivisionsY = math.round((0.5d+(aabb.height/delta)).toFloat)
    if (true){
      println("subdivisionsX=" + subdivisionsX)
      println("subdivisionsY=" + subdivisionsY)
      println("delta=" + delta)
      if (delta*subdivisionsX < aabb.width) println("delta*subdivisionsX is too small")
      if (delta*subdivisionsY < aabb.height) println("delta*subdivisionsY is too small")
    }
    if (false){  
      println("reducedAABB=" + reducedAABB)
      //println("reducedClipPolygon=" + reducedClipPolygon.vertices.mkString(","))
      println("reducedClipPolygon.isClockwise=" + reducedClipPolygon.isClockwise)
      println("reducedClipPolygon.isSelfIntersecting=" + reducedClipPolygon.isSelfIntersecting)
      println("reducedAABB.center=" + reducedAABB.x + " " + reducedAABB.y)
      println("reducedClipPolygon.minCenterDistanceSquared=" + reducedClipPolygon.minCenterDistanceSquared)
  
      println("reducedAABB=" + reducedAABB)
      println("reducedAABB.width=" + reducedAABB.width)
      println("reducedAABB.height=" + reducedAABB.height)
    
    }
    
    for (xp <- 0 until subdivisionsX; yp <-0 until subdivisionsY) yield {
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
      
      val p = Polygon2D(IndexedSeq(p3, p2, p1, p0))
      assert(p.isClockwise)
      val intersects = p.intersects(reducedClipPolygon) 
      if (false){
        val i1 = p.intersects(reducedClipPolygon)
        val i2 = p.intersections(reducedClipPolygon)
        println("intersects claims:" + i1)
        if (i1 != (i2.size>0)) {
          
          println("intersections claims:" + i2)
        }
        assert(i1 == (i2.size>0))
        i1
      }
      
      if (cp0 && cp1 && cp2 && cp3 && !intersects) {
        
        // p is cleanly inside the clip polygon
        val p03d = toTVec3D(p0)
        val p13d = toTVec3D(p1)
        val p23d = toTVec3D(p2)
        val p33d = toTVec3D(p3)
        // try to make the triangulation parallel with the circumference
        if ({ val direction = p1.sub(center); direction.x*direction.y<0}) {  
          rvMesh.addFace(p23d, p03d, p13d)
          rvMesh.addFace(p03d, p23d, p33d)
        } else {
          rvMesh.addFace(p33d, p13d, p23d)
          rvMesh.addFace(p33d, p03d, p13d)
        }
        
        //val p = new Polygon2D(Array(p0, p1, p2, p3))
        //println("new Polygon p0, p1, p3, p2 is clockwise: " + p.isClockwise)
        //println("new Polygon p0, p1, p3, p2 is isSelfIntersecting: " + p.isSelfIntersecting)
        //println("new Polygon2D is clockwise: " + new Polygon2D(Array(p1, p3, p2).iterator).isClockwise()) 
      } else if (intersects) { 
        
        //println("p is clockwise: " + p.isClockwise)
        //println("p is selfintersecting: " + p.isSelfIntersecting)
            
        //println("new Polygon2D is clockwise: " + new Polygon2D(p.iterator).isClockwise())
        val clippings = WeilerAthertonClipper.clip(p, reducedClipPolygon, Polygon2D.ε )
        if (clippings.size == 1 && clippings(0) == p) {
          // every vertex survived clipping intact
          val p03d = toTVec3D(p0)
          val p13d = toTVec3D(p1)
          val p23d = toTVec3D(p2)
          val p33d = toTVec3D(p3)
          rvMesh.addFace(p23d, p03d, p13d)
          rvMesh.addFace(p03d, p23d, p33d)
        } else clippings.filter(c => c.size>=3).foreach(clipped=>{ 
          
          if (clipped.size == 4 ) {
            val v = clipped.vertices
            val p03d = toTVec3D(clipped.vertices(0))
            val p13d = toTVec3D(clipped.vertices(1))
            val p23d = toTVec3D(clipped.vertices(2))
            val p33d = toTVec3D(clipped.vertices(3))
            
            if (!Polygon2D.isClockwise(v(0), v(1), v(2))){ // flip this if you want to render triangulated faces backwards (for debugging) 
              rvMesh.addFace(p03d, p13d, p23d)
            } else {
              rvMesh.addFace(p03d, p23d, p13d)
            }
            
            if (!Polygon2D.isClockwise(v(0), v(2), v(3))){ // flip this if you want to render triangulated faces backwards (for debugging) 
              rvMesh.addFace(p03d, p23d, p33d)
            } else {
              rvMesh.addFace(p03d, p33d, p23d)
            }
            
          } else {
            val triangles = triangulator.triangulatePolygon(clipped)
            triangles.foreach(t =>{
              val p03d = toTVec3D(t(0))
              val p13d = toTVec3D(t(1))
              val p23d = toTVec3D(t(2))
              if (!Polygon2D.isClockwise(t(0), t(1), t(2))){ // flip this if you want to render triangulated faces backwards (for debugging) 
                rvMesh.addFace(p03d, p13d, p23d)
              } else {
                rvMesh.addFace(p03d, p23d, p13d)
              }
            })
          }
        })
      } /* else {  // cleanly outside the reduced clip polygon
        println("p=" + p)
        println("reducedClipPolygon=" + reducedClipPolygon)
        p.toMesh(p.getCentroid,(a:Vec2D,b:Vec2D,c:Vec2D) => {
                rvMesh.addFace(new TVec3D(a.x.toFloat, a.y.toFloat, 0f),
                               new TVec3D(b.x.toFloat, b.y.toFloat, 0f),
                               new TVec3D(c.x.toFloat, c.y.toFloat, 0f))
              })
              
        //assert(false)
      }*/
    }
    if (false) {
        // perform extra sanity check on the result
        val fuubarVertices = new ArrayBuffer[Vec2D]
        rvMesh.vertices.foreach(v => {
          val v2 = Vec2D(v._1.x, v._1.y)
          if (!clockwiseClipPolygon.containsPoint(v2)) fuubarVertices.append(v2)
        })
        if (fuubarVertices.size!=0) {
          //println("subjectEdges" + subjectEdges)
          println("reducedClipPolygon" + reducedClipPolygon)
          println("produces wierd data:" + fuubarVertices.mkString(","))
        }
      }
     
    rvMesh
  }
  
  /**
   * 
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
  
  def processData(edges:Polygon2DConverter, center:Option[ReadonlyVec3D], subdivisions:Int, calculator:ZCalculator, useMultiThreading:Boolean) : Mesh3DConverter = {
    
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
    
    val (sqrtThreads,deltasPerThread) = subdivisions match {
      case 0 => (1,1) // should really never happen
      case 1 => (2,1) // gives 2*2 running threads
      case 2 => (2,2) // gives 2*2 running threads 
      case _ => (4,(8+subdivisions)/4) // gives 4*4 running threads.
    }
    
    val convexHullPolygon = polygon.toConvexHull2 (Option(true))
    
    val aabb = convexHullPolygon.bounds
    val aabbmax = if (aabb.width > aabb.height) aabb.width else aabb.height
    val delta = aabbmax / (1d + subdivisions)  
    val distancePerThread = delta*deltasPerThread
    
    if (distancePerThread*sqrtThreads*(1d+subdivisions) == aabb.width || distancePerThread*sqrtThreads*(1d+subdivisions) == aabb.height)
      println("something does now add up\n" + 
              "sqrtThreads=" + sqrtThreads + "\n" + 
              "distancePerThread=" + distancePerThread + "\n" + 
              "distancePerThread*sqrtThreads*(1+subdivisions)=" + distancePerThread*(1d+subdivisions) + " aabb.width=" + aabb.width + "\n" + 
              "distancePerThread*sqrtThreads*(1+subdivisions)=" + distancePerThread*(1d+subdivisions) + " aabb.height=" + aabb.height)
    
    println("aabb=" + aabb)
    println("aabb.width/delta=" + aabb.width/delta  + " aabb.height/delta=" +  aabb.height/delta)
    println("distancePerThread=" + distancePerThread)
    println("deltasPerThread=" + deltasPerThread)
    println("delta=" + delta)
    
    val realCenter = if (center.isDefined) {
      val c = center.get
      Vec2D(c.x*scale, c.y*scale)
    } else {
      convexHullPolygon.getCentroid
    }
    //println("realCenter=" + realCenter)
    //val realCenter2d = new Vec2D(realCenter3d.x, realCenter3d.y)
    //val realInverseCenter2d = realCenter2d.scale(-1)
    
    val resultingMesh = if (useMultiThreading && subdivisions>0) {
      val trueSqrtThreads = (0.5 + aabbmax / distancePerThread).toInt
      val job = for (i <- 0 until trueSqrtThreads; j <- 0 until trueSqrtThreads ) yield {
        AABB2D(aabb.min.x+distancePerThread*i, aabb.min.y+distancePerThread*j, aabb.min.x+distancePerThread*(i+1), aabb.min.y+distancePerThread*(j+1))
      }
      //println("distancePerThread=" + subdivisions*delta/(threads/2d))
      println("subjobs:\n" + job.mkString("\n") + "\n")
      job.par.map(j=>processDataPerThread(polygon,j,realCenter,delta)).seq.foldLeft(new TriangleMesh)((rv,part)=>rv.addMesh(part))
    } else {
      processDataPerThread(polygon,aabb,realCenter,delta)
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
    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitIsMetric = options.getUnitSystemProperty(traceMsg)
    val zAlgorithm = options.getStringProperty("zAlgorithm", "CIRCLEARC")
    val (radius1Property,radius2Property) = inAscendingOrder( options.getFloatProperty("radius1Property", 0f, traceMsg),
                                                              options.getFloatProperty("radius2Property", 1f, traceMsg) )
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
      returnMessageBuilder.addModels(processData(edgePolygon, center, subdivisions, calculator, useMultiThreading).toPBModel(None, None))
      returnMessageBuilder
    })
  }
}

