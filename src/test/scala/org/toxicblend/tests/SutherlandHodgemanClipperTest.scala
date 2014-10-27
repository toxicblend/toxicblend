package org.toxicblend.tests


import org.scalatest._
import org.toxicblend.operations.meshgenerator.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.operations.meshgenerator.vecmath.ImmutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.MutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.Vec2D
import org.toxicblend.operations.meshgenerator.vecmath.Line2D
import toxi.geom.Polygon2D
import org.toxicblend.attic.{CyclicTree,Payload}
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class SutherlandHodgemanClipperTest extends FlatSpec with Matchers {
  
  val floatTolerance = 0.0001f
  
  def indexByHeading(seq:IndexedSeq[Vec2D] ) : IndexedSeq[Vec2D] = {
    val angleSequence = seq.map(v => new Payload(v.heading, 0, v) )
    //println(a.map(p=>p.pos).mkString(","))
    //println(a.map(p=>p.angle*180d/math.Pi).mkString(","))
    val rv = CyclicTree.inOrder(angleSequence)._1.map( p => p.pos )
    //println(rv.map(v=>"" + v + "@" + v.heading*180d/math.Pi).mkString(","))
    rv
  }
  
  /** 
   *   ____  /          ____ 
   *  |    |/          |    |
   *  |    /       =>  |   /
   *  |   /|           |  /
   *  |_/__|           |_/
   *   /   
   */
  "SutherlandHodgemanClipperTest-1" should "clip just fine" in {
    
    val p0 = Vec2D(2,-1):Vec2D
    val p1 = Vec2D(-1,-1):Vec2D
    val p2 = Vec2D(-1,1):Vec2D
    val p3 = Vec2D(2,1):Vec2D
    val i0 = Vec2D(2,0):Vec2D
    val i1 = Vec2D(1,-1):Vec2D
    
    val iLine = new Line2D(i1, i0)
    val polygon = ArrayBuffer(p0, p1, p2, p3)
    
    val clipped = indexByHeading(SutherlandHodgemanClipper.clip(polygon, iLine).toIndexedSeq)
    //println(clipped.mkString(","))
    clipped.size should be (5)
    clipped.get(0) should be (p2)
    clipped.get(1) should be (p3)
    clipped.get(2) should be (i0)
    clipped.get(3) should be (i1)
    clipped.get(4) should be (p1)
  }
  
  /** 
   *   ____  /             /
   *  |    |/             /|
   *  |    /       =>    / |
   *  |   /|            /  |
   *  |_/__|           /___|
   *   /              /               
   */
  "SutherlandHodgemanClipperTest-2" should "clip just fine" in {
    
    val p0 = Vec2D(2,-1)
    val p1 = Vec2D(-1,-1)
    val p2 = Vec2D(-1,1)
    val p3 = Vec2D(2,1)
    val i0 = Vec2D(1,1)
    val i1 = Vec2D(-1,0)
    
    val iLine = new Line2D(i0, i1)
    val polygon = ArrayBuffer(p0, p1, p2, p3)
    val clipper = new SutherlandHodgemanClipper
    
    val clipped = indexByHeading(clipper.clipPolygon(polygon, iLine).toIndexedSeq)
    //println("SutherlandHodgemanClipperTest-2: polygon=" + clipped.mkString(","))
    //println("SutherlandHodgemanClipperTest-2: clipline=" + iLine )
    //println("SutherlandHodgemanClipperTest-2: clipped=" + clipped.mkString(","))
    //println("SutherlandHodgemanClipperTest-2: expected=" + Array(i1,i0,p3,p0,p1).mkString(","))
    clipped.size should be (5)
    clipped.get(0) should be (i1)
    clipped.get(1) should be (i0)
    clipped.get(2) should be (p3)
    clipped.get(3) should be (p0)
    clipped.get(4) should be (p1)
  }
    
  /**
   * Rectangular, clockwise clipping 
   *
  "SutherlandHodgemanClipperTest-3" should "clip just fine" in {
    
    val p0 = Vec2D(1,-1)
    val p1 = Vec2D(-1,-1)
    val p2 = Vec2D(-1,1)
    val p3 = Vec2D(1,1)
    val trace = true
    
    var polygon:IndexedSeq[Vec2D] = ArrayBuffer(p0, p1, p2, p3).map(v=>v.scale(2))
    val clipper = new SutherlandHodgemanClipper
    
    if (trace) {
      println("SutherlandHodgemanClipperTest-3 polygon=" + polygon)
      println
    }
    
    var iLine = new Line2D(p0, p1)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    iLine = new Line2D(p1, p2)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    iLine = new Line2D(p2, p3)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    iLine = new Line2D(p3, p0)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    val rv = indexByHeading(polygon)
    if (trace) {
      println("rv=" + rv)
      println
    }
    rv.size should be (4)
    rv.get(0) should be (p2)
    rv.get(1) should be (p3)
    rv.get(2) should be (p0)
    rv.get(3) should be (p1)
  }
  
  **
   * Rectangular, counter-clockwise clipping 
   *
  "SutherlandHodgemanClipperTest-4" should "clip just fine" in {
    
    val p0 = Vec2D(1,-1)
    val p1 = Vec2D(-1,-1)
    val p2 = Vec2D(-1,1)
    val p3 = Vec2D(1,1)
    val trace = true
    
    var polygon:IndexedSeq[Vec2D] = ArrayBuffer(p2, p1, p0, p3).map(v=>v.scale(10))
    val clipper = new SutherlandHodgemanClipper
    
    if (trace) {
      println("SutherlandHodgemanClipperTest-4 polygon=" + polygon)
      println
    }
    
    var iLine = new Line2D(p0, p1)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    iLine = new Line2D(p1, p2)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    iLine = new Line2D(p2, p3)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    iLine = new Line2D(p3, p0)
    polygon = clipper.clipPolygon(polygon, iLine)
    if (trace) {
      println("iLine=" + iLine)
      println("polygon=" + polygon)
      println
    }
    polygon.size should be (4)
    
    val rv = indexByHeading(polygon.toIndexedSeq)
    println(rv)
    rv.size should be (4)
    rv.get(0) should be (p1)
    rv.get(1) should be (p0)
    rv.get(2) should be (p3)
    rv.get(3) should be (p2)
  }
  
  **
   *  Rectangular, counter-clockwise, coincident clipping 
   *
  "SutherlandHodgemanClipperTest-5" should "clip counter-clockwise, coincident polygons" in {
    
    val p0 = Vec2D(1,-1)
    val p1 = Vec2D(-1,-1)
    val p2 = Vec2D(-1,1)
    val p3 = Vec2D(1,1)
    
    
    var polygon:IndexedSeq[Vec2D] = ArrayBuffer(p2.scale(10), p1.scale(10), p0.scale(10), p3.scale(10))
    val clipper = new SutherlandHodgemanClipper
    
    //println(polygon)
    //println
    
    var iLine = new Line2D(p0, p1)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    iLine = new Line2D(p1, p2)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    iLine = new Line2D(p2, p3)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    iLine = new Line2D(p3, p0)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    val rv = indexByHeading(polygon.toIndexedSeq)
    //println(rv)
    rv.size should be (4)
    rv.get(0) should be (p1)
    rv.get(1) should be (p0)
    rv.get(2) should be (p3)
    rv.get(3) should be (p2)
  }
  
  **
   *  Rectangular, coincident, clockwise clipping 
   *
  "SutherlandHodgemanClipperTest-6" should "clip clockwise, coincident polygons" in {
    
    val p0 = Vec2D(1,-1)
    val p1 = Vec2D(-1,-1)
    val p2 = Vec2D(-1,1)
    val p3 = Vec2D(1,1)
    
    var polygon:IndexedSeq[Vec2D] = ArrayBuffer(p0, p1, p2, p3)
    val clipper = new SutherlandHodgemanClipper
    
    //println(polygon)
    //println
    
    var iLine = new Line2D(p0, p1)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    iLine = new Line2D(p1, p2)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    iLine = new Line2D(p2, p3)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    iLine = new Line2D(p3, p0)
    polygon = clipper.clipPolygon(polygon, iLine)
    //println(iLine)
    //println(polygon)
    //println
    
    val rv = indexByHeading(polygon.toIndexedSeq)
    //println(rv)
    rv.size should be (4)
    rv.get(0) should be (p2)
    rv.get(1) should be (p3)
    rv.get(2) should be (p0)
    rv.get(3) should be (p1)
  }
 
  */
  
  /**
   *  Rectangular clipping from http://rosettacode.org/wiki/Sutherland-Hodgman_polygon_clipping
   */
  "SutherlandHodgemanClipperTest-7" should "clip" in {
      
    val polygon = ArrayBuffer((50,150),(200,50),(350,150),(350,300),(250,300),(200,250),(150,350),(100,250),(100,200)).map(p=>Vec2D(p._1,p._2))
    val clipEdges = ArrayBuffer((100,100),(300,100),(300,300),(100,300)).map(p=>Vec2D(p._1,p._2))
    polygon += polygon.head
    clipEdges += clipEdges.head
    
    //println("polygon: " + polygon)
    //println("clipEdges: " + clipEdges)
        
    //println("polygon: " + polygon)
    //println("clipEdges: " + clipEdges)
    //println("center: " + center)
    //println
    val clipped = clipEdges.sliding(2).foldLeft(polygon:IndexedSeq[Vec2D])((x,e) => {
      val edge = new Line2D(e.head, e.last)
      val p = SutherlandHodgemanClipper.clip(x, edge)
      //println("clipped with : " + edge)
      //println("result : " + p)
      p
    })
    
    val correctAnswer = Array((100.000000, 116.666667),
                              (125.000000, 100.000000),
                              (275.000000, 100.000000),
                              (300.000000, 116.666667),
                              (300.000000, 300.000000),
                              (250.000000, 300.000000),
                              (200.000000, 250.000000),
                              (175.000000, 300.000000),
                              (125.000000, 300.000000),
                              (100.000000, 250.000000)).map(p=>Vec2D(p._1.toFloat, p._2.toFloat))
    clipped.size should be (correctAnswer.size)
    (0 until clipped.size).foreach(i=>{
      clipped(i).x should be ( correctAnswer(i).x plusOrMinus floatTolerance)
      clipped(i).y should be ( correctAnswer(i).y plusOrMinus floatTolerance)
    })
  }
}