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
}