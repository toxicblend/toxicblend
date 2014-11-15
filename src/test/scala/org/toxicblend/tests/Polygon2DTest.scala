package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.vecmath.MutableVec2D
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.AABB2D
import org.toxicblend.vecmath.FiniteLine2D
import org.toxicblend.vecmath.Polygon2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class Polygon2DTest extends VecMathBaseTest {
  
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
  
  "Polygon2DTest-8" should "test equals" in {
    var p1 = toPolygon2D(Seq((0d,0d),(3d,0d),(3d,3d),(1d,1d),(0d,3d)))
    var p2 = toPolygon2D(Seq((0d,0d),(3d,0d),(3d,3d),(1d,1d),(0d,3d)))
    (0 until 6).foreach(i=>{
      p1==p2 should be (true)
      p2==p1 should be (true)

      p1 = p1.shift1
      p2 = p2.shift1
      p2 = p2.shift1
    }) 
  }
  
  "Polygon2DTest-9" should "test hasCollinearSameDirection" in {
    var p1 = toPolygon2D(Seq((0d,0d),(3d,0d),(3d,3d),(2d,2d),(1d,1d),(0d,3d)))
    var p2 = toPolygon2D(Seq((0d,0d),(3d,0d),(3d,3d),(1d,1d),(0d,3d)))
    (0 until 6).foreach(i=>{
      p1.hasCollinearSameDirection should be (true)
      p1.isSimple should be (false)
      p2.hasCollinearSameDirection should be (false)
      p2.isSimple should be (true)
      p1 = p1.shift1
      p2 = p2.shift1
    }) 
  }
  
  "Polygon2DTest-10" should "test intersect" in {
    var p1 = AABB2D(100,100,200,200).toPolygon2D(true)
    var p2 = toPolygon2D(Seq((110d,0d),(110d,310d),(190d,0d)))
    
    (0 until 4).foreach(i=>{
      //println("p1:" + p1)
      //println("p2:" + p2)
      (0 until 3).foreach(j=>{
        p1.isSimple should be (true)
        p1.intersects(p2) should be (true)
        p2.intersects(p1) should be (true)
        p2.isSimple should be (true)
        p2 = p2.shift1
      })
      p1 = p1.shift1
    }) 
  }
  
}