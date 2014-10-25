package org.toxicblend.tests


import org.scalatest._
import org.toxicblend.operations.meshgenerator.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.operations.meshgenerator.vecmath.ImmutableVertex2D
import org.toxicblend.operations.meshgenerator.vecmath.MutableVertex2D
import org.toxicblend.operations.meshgenerator.vecmath.Vertex2D
import org.toxicblend.operations.meshgenerator.vecmath.Line2D
import toxi.geom.Polygon2D
import org.toxicblend.attic.{CyclicTree,Payload}
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class SutherlandHodgemanClipperTest extends FlatSpec with Matchers {
  
  val floatTolerance = 0.0001f
  
  def indexByHeading(seq:IndexedSeq[Vertex2D] ) : IndexedSeq[Vertex2D] = {
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
    
    val p0 = ImmutableVertex2D(2,-1):Vertex2D
    val p1 = ImmutableVertex2D(-1,-1):Vertex2D
    val p2 = ImmutableVertex2D(-1,1):Vertex2D
    val p3 = ImmutableVertex2D(2,1):Vertex2D
    val i0 = ImmutableVertex2D(2,0):Vertex2D
    val i1 = ImmutableVertex2D(1,-1):Vertex2D
    
    val iLine = new Line2D(i1, i0)
    val polygon = ArrayBuffer(p0, p1, p2, p3, p0)
    val clipper = new SutherlandHodgemanClipper
    
    val clipped = indexByHeading(clipper.clipPolygon(polygon, iLine).toIndexedSeq)
    println(clipped.mkString(","))
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
    
    val p0 = ImmutableVertex2D(2,-1)
    val p1 = ImmutableVertex2D(-1,-1)
    val p2 = ImmutableVertex2D(-1,1)
    val p3 = ImmutableVertex2D(2,1)
    val i0 = ImmutableVertex2D(1,1)
    val i1 = ImmutableVertex2D(-1,0)
    
    val iLine = new Line2D(i0, i1)
    val polygon = ArrayBuffer(p0, p1, p2, p3, p0)
    val clipper = new SutherlandHodgemanClipper
    
    val clipped = indexByHeading(clipper.clipPolygon(polygon, iLine).toIndexedSeq)
    println("SutherlandHodgemanClipperTest-2:" + clipped.mkString(","))
    clipped.size should be (6)
    clipped.get(0) should be (i1)
    clipped.get(1) should be (i0)
    clipped.get(2) should be (p3)
    clipped.get(3) should be (p0)
    clipped.get(4) should be (p1)
    clipped.get(5) should be (p0)
  }
    
  /**
   *  Rectangular, clockwise clipping 
   */
  "SutherlandHodgemanClipperTest-3" should "clip just fine" in {
    
    val p0 = ImmutableVertex2D(1,-1)
    val p1 = ImmutableVertex2D(-1,-1)
    val p2 = ImmutableVertex2D(-1,1)
    val p3 = ImmutableVertex2D(1,1)
    
    
    var polygon:IndexedSeq[Vertex2D] = ArrayBuffer(p0.scale(10), p1.scale(10), p2.scale(10), p3.scale(10), p0.scale(10))
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
    
    val rv = indexByHeading(polygon)
    //println(rv)
    rv.size should be (4)
    rv.get(0) should be (p2)
    rv.get(1) should be (p3)
    rv.get(2) should be (p0)
    rv.get(3) should be (p1)
  }
  
  /**
   *  Rectangular, counter-clockwise clipping 
   */
  "SutherlandHodgemanClipperTest-4" should "clip just fine" in {
    
    val p0 = ImmutableVertex2D(1,-1)
    val p1 = ImmutableVertex2D(-1,-1)
    val p2 = ImmutableVertex2D(-1,1)
    val p3 = ImmutableVertex2D(1,1)
    
    
    var polygon:IndexedSeq[Vertex2D] = ArrayBuffer(p2.scale(10), p1.scale(10), p0.scale(10), p3.scale(10))
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
  
  /**
   *  Rectangular, counter-clockwise, coincident clipping 
   */
  "SutherlandHodgemanClipperTest-5" should "clip counter-clockwise, coincident polygons" in {
    
    val p0 = ImmutableVertex2D(1,-1)
    val p1 = ImmutableVertex2D(-1,-1)
    val p2 = ImmutableVertex2D(-1,1)
    val p3 = ImmutableVertex2D(1,1)
    
    
    var polygon:IndexedSeq[Vertex2D] = ArrayBuffer(p2.scale(10), p1.scale(10), p0.scale(10), p3.scale(10))
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
  
  /**
   *  Rectangular, coincident, clockwise clipping 
   */
  "SutherlandHodgemanClipperTest-6" should "clip clockwise, coincident polygons" in {
    
    val p0 = ImmutableVertex2D(1,-1)
    val p1 = ImmutableVertex2D(-1,-1)
    val p2 = ImmutableVertex2D(-1,1)
    val p3 = ImmutableVertex2D(1,1)
    
    
    var polygon:IndexedSeq[Vertex2D] = ArrayBuffer(p0, p1, p2, p3)
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
  
  /**
   *  Rectangular clipping from http://rosettacode.org/wiki/Sutherland-Hodgman_polygon_clipping
   */
  "SutherlandHodgemanClipperTest-7" should "clip" in {
      
    val polygon = ArrayBuffer((50,150),(200,50),(350,150),(350,300),(250,300),(200,250),(150,350),(100,250),(100,200)).map(p=>ImmutableVertex2D(p._1,p._2))
    val clipEdges = ArrayBuffer((100,100),(300,100),(300,300),(100,300)).map(p=>ImmutableVertex2D(p._1,p._2))
    polygon += polygon.head
    clipEdges += clipEdges.head
    val center = clipEdges.foldLeft(new MutableVertex2D)((x,s)=> x.addSelf(s)).scaleSelf(1f/clipEdges.size.toFloat)
    
    //println("polygon: " + polygon)
    //println("clipEdges: " + clipEdges)
    //println("center: " + center)
    
    val clipper = new SutherlandHodgemanClipper
    
    //println("polygon: " + polygon)
    //println("clipEdges: " + clipEdges)
    //println("center: " + center)
    //println
    val clipped = clipEdges.sliding(2).foldLeft(polygon:IndexedSeq[Vertex2D])((x,e) => {
      val edge = new Line2D(e.head, e.last)
      val p = clipper.clipPolygon(x, edge)
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
                              (100.000000, 250.000000)).map(p=>ImmutableVertex2D(p._1.toFloat, p._2.toFloat))
    clipped.size should be (correctAnswer.size)
    (0 until clipped.size).foreach(i=>{
      clipped(i).x should be ( correctAnswer(i).x plusOrMinus floatTolerance)
      clipped(i).y should be ( correctAnswer(i).y plusOrMinus floatTolerance)
    })
  }
}