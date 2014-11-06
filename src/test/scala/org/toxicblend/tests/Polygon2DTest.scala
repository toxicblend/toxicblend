package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.operations.meshgenerator.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.operations.meshgenerator.vecmath.MutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.Vec2D
import org.toxicblend.operations.meshgenerator.vecmath.FiniteLine2D
import org.toxicblend.operations.meshgenerator.vecmath.Polygon2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class Polygon2DTest extends FlatSpec with Matchers {
  
  def toPolygon2D(seq:Seq[(Double,Double)], scale:Double=1d, x:Double=0d, y:Double=0d):Polygon2D = {
    Polygon2D(seq.toIndexedSeq.map(p=>Vec2D(p._1*scale+x,p._2*scale+y)))
  }
  
  "Polygon2DTest-1" should "detect inside just fine" in {
    val s = Array((100.0,200.0),(158.77852522924732,180.90169943749476),
                  (195.10565162951536,130.90169943749476),(195.10565162951536,100.0),
                  (300.0,100.0),(300.0,300.0),(100.0,300.0)).map(v=>Vec2D(v._1, v._2))
    val p = Polygon2D(s)
    p.containsPoint(Vec2D(100,100)) should be (false)
  }
  
  "Polygon2DTest-2" should "test isConvex" in {
    var p = toPolygon2D(Seq((0,0),(1,0),(1,1),(0,1)))
    (0 until 5).foreach(i=>{
      //println(p.vertices.mkString(","))
      p.isConvex should be (true)
      p = p.shift1
    }) 
  }
  
  "Polygon2DTest-3" should "test isConvex" in {
    var p = toPolygon2D(Seq((0,0),(2,0),(2,2),(1,1),(0,2)))
    (0 until 6).foreach(i=>{
      //println(p.vertices.mkString(","))
      p.isConvex should be (false)
      p = p.shift1
    }) 
  }
  
  "Polygon2DTest-4" should "test convexHull" in {
    var p = toPolygon2D(Seq((0,0),(3,0),(3,3),(1,1),(0,3)))
    var c = p
    (0 until 6).foreach(i=>{
      p.isClockwise should be (false)
      p.isConvex should be (false)
      p.isSelfIntersecting should be (false)
      
      c = p.toConvexHull(Option(true))
      //println(c.vertices.mkString(","))
      c.size should be (4)
      c.isConvex should be (true)
      c.isClockwise should be (true)
      
      p = p.shift1
    }) 
  }
  
  "Polygon2DTest-5" should "test convexHull" in {
    var p = toPolygon2D(Seq((0d,0d),(3d,0d),(3d,3d),(1d,1d),(0d,3d)).reverse)
    var c = p
    (0 until 6).foreach(i=>{
      p.isClockwise should be (true)
      p.isConvex should be (false)
      p.isSelfIntersecting should be (false)
      
      c = p.toConvexHull(Option(false))
      //println(c.vertices.mkString(","))
      c.size should be (4)
      c.isConvex should be (true)
      c.isClockwise should be (false)
      
      p = p.shift1
    }) 
  }
  
  "Polygon2DTest-6" should "test convexHull2" in {
    var p = toPolygon2D(Seq((0d,0d),(3d,0d),(3d,3d),(1d,1d),(0d,3d)))
    var c = p
    (0 until 6).foreach(i=>{
      p.isClockwise should be (false)
      p.isConvex should be (false)
      p.isSelfIntersecting should be (false)
      
      c = p.toConvexHull2(Option(false))
      //println(c.vertices.mkString(","))
      c.size should be (4)
      c.isConvex should be (true)
      c.isClockwise should be (false)
      
      p = p.shift1
    }) 
  }
  
  "Polygon2DTest-7" should "test convexHull2" in {
    var p = toPolygon2D(Seq((0d,0d),(3d,0d),(3d,3d),(1d,1d),(0d,3d)).reverse)
    var c = p
    (0 until 6).foreach(i=>{
      p.isClockwise should be (true)
      p.isConvex should be (false)
      p.isSelfIntersecting should be (false)
      
      c = p.toConvexHull2(Option(true))
      //println(c.vertices.mkString(","))
      c.size should be (4)
      c.isConvex should be (true)
      c.isClockwise should be (true)
      
      p = p.shift1
    }) 
  }
}