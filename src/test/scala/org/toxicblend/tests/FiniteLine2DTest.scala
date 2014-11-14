package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.vecmath.MutableVec2D
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.FiniteLine2D
import org.toxicblend.vecmath.Polygon2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class FiniteLine2DTest extends VecMathBaseTest {
  
  "FiniteLine2DTest-1" should "test areCollinear" in {
    var p = toPolygon2D(Seq((50.0,75.0), (100.0,50.0), (50.0,25.0)))
    (0 until 4).foreach(i=>{
      p.hasCollinearSameDirection should be (false)
      //println("FiniteLine2DTest-1: areCollinear?" + p.vertices)
      FiniteLine2D.areCollinear(p.vertices(0), p.vertices(1), p.vertices(2)) should be (false)
      p = p.shift1
    }) 
  }
  
  "FiniteLine2DTest-2" should "test areCollinear" in {
    var p = toPolygon2D(Seq((1.0,4.0), (4.0,16.0), (2.0,8.0), (100,100)))
    (0 until 4).foreach(i=>{
      //println("FiniteLine2DTest-2: areCollinear?" + p.vertices)
      p.hasCollinearSameDirection should be (false)
      p.hasCollinear should be (true)
      //FiniteLine2D.areCollinear(p.vertices(0), p.vertices(1), p.vertices(2)) should be (false)
      p = p.shift1
    }) 
  }
  
  "FiniteLine2DTest-2" should "test intersects and intersectLine" in {
    val l1b = FiniteLine2D(0,0,1,0)
    val l2b = FiniteLine2D(2,-1,2,1)
    val l3b = FiniteLine2D(.5,-1,.5,1)
    
    (0 until 10).foreach(i=> {
      val l1 = l1b.add(Vec2D(0.1*i, -0.1*i))
      val l2 = l2b.add(Vec2D(0.1*i, -0.1*i))
      val l3 = l3b.add(Vec2D(0.1*i, -0.1*i))
      
      println("l1=" + l1)
      println("l2=" + l2)
      println("l3=" + l3)
      
      l1.intersectLine(l2).isDefined should be (false)
      l2.intersectLine(l1).isDefined should be (false)
      l1.intersects(l2) should be (false)
      l2.intersects(l1) should be (false)
      
      
      l1.intersectLine(l3).isDefined should be (true)
      l3.intersectLine(l1).isDefined should be (true)
      l1.intersects(l3) should be (true)
      l3.intersects(l1) should be (true)
    })
  }
}

/*
object FiniteLine2DTestApp extends App {
  val l1b = FiniteLine2D(0,0,1,0)
  val l2b = FiniteLine2D(2,-1,2,1)
  val l3b = FiniteLine2D(.5,-1,.5,1)
  
  (0 until 10).foreach(i=> {
    val l1 = l1b.add(Vec2D(0.1*i, 0.1*i))
    val l2 = l2b.add(Vec2D(0.1*i, 0.1*i))
    val l3 = l3b.add(Vec2D(0.1*i, 0.1*i))
    
    println("l1=" + l1)
    println("l2=" + l2)
    println("l3=" + l3)
    
    if (l1.intersectLine(l2).isDefined != false)
      println("bug")
    if (l2.intersectLine(l1).isDefined != false)
      println("bug")
    if (l1.intersects(l2) != false)
      println("bug")
    if (l2.intersects(l1) != false)
      println("bug")
    
    
    if (l1.intersectLine(l3).isDefined != true)
      println("bug")
    if (l3.intersectLine(l1).isDefined != true)
      println("bug")
    if (l1.intersects(l3) != true)
      println("bug")
    if (l3.intersects(l1) != true)
      println("bug")
  })
 
}
*/