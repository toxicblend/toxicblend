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

class Vec2DTest extends VecMathBaseTest {
  
  val tolerance = 0.000001d
  
  "Vec2DTest-1" should "test cross product" in {
    val a = Vec2D(2,1)
    val b = Vec2D(1,2)
    val o = Vec2D(1,1)
    val oa = a.sub(o)
    val ob = b.sub(o)
    
    Vec2D.cross(o, a, b) should be (oa.cross(ob) plusOrMinus tolerance)
    Vec2D.cross(oa.x, oa.y, ob.x, ob.y) should be (oa.cross(ob) plusOrMinus tolerance)
  }
  
}