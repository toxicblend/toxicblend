package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.vecmath.MutableVec2D
import org.toxicblend.util.NumberUtils.d2r
import org.toxicblend.util.NumberUtils.r2d
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.AABB2D
import org.toxicblend.vecmath.FiniteLine2D
import org.toxicblend.vecmath.Polygon2D
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class Vec2DTest extends VecMathBaseTest {
  
  val tolerance = 0.000001d
  
  "Vec2DTest-1" should "test cross product" in {
    val o = Vec2D(1.2,1.1)
    val aα = 0.1
    val bα = 0.3
  
    val aMag = 1.5
    for (αDeg <- 0 to 360) {
      val α = d2r(αDeg)
      val a = Vec2D(o.x + aMag * math.cos(α + aα) , o.y + aMag *math.sin(α + aα))
      val b = Vec2D(o.x + aMag * math.cos(α + bα) , o.y + aMag *math.sin(α + bα))
      val oa = a.sub(o)
      val ob = b.sub(o)
    
      Vec2D.cross(o, a, b) should be (oa.cross(ob) plusOrMinus tolerance)
      Vec2D.cross(oa.x, oa.y, ob.x, ob.y) should be (oa.cross(ob) plusOrMinus tolerance)
    }    
  }
  
  "Vec2DTest-2" should "test dot product" in {
    
    val o = Vec2D(1.2,1.1)
    val aα = 0.1
    val bα = 0.3
  
    val aMag = 1.5
    for (αDeg <- 0 to 360 ) {
      val α = d2r(αDeg)
      val a = Vec2D(o.x + aMag * math.cos(α + aα) , o.y + aMag *math.sin(α + aα))
      val b = Vec2D(o.x + aMag * math.cos(α + bα) , o.y + aMag *math.sin(α + bα))
      val oa = a.sub(o)
      val ob = b.sub(o)
    
      Vec2D.dot(o, a, b) should be (oa.dot(ob) plusOrMinus tolerance)
      Vec2D.dot(oa.x, oa.y, ob.x, ob.y) should be (oa.dot(ob) plusOrMinus tolerance)
      math.sqrt(Vec2D.normalizedDotSquared(o,a,b)) should be (oa.dot(ob) / (oa.magnitude * ob.magnitude)  plusOrMinus tolerance)  
      //math.acos(math.sqrt(Vec2D.normalizedDotSquared(o,a,b))) should be oa  plusOrMinus tolerance)  
    }
  }
}

object Vec2DTest extends App {
   val o = Vec2D(1.2,1.1)
    val aα = 0.1
    val bα = 0.0
  
    val aMag = 1.5
    for (αDeg <- 0 to 360 by 15) {
      val α = d2r(αDeg)
      val a = Vec2D(o.x + 1 , o.y + 0d)
      val b = Vec2D(o.x + aMag * math.cos(α + bα) , o.y + aMag *math.sin(α + bα))
      val oa = a.sub(o)
      val ob = b.sub(o)
      val ndsws = Vec2D.normalizedDotSquaredWithSign(o,a,b)
      val angle = if (ndsws <0) math.acos(-math.sqrt(-ndsws)) else math.acos(math.sqrt(ndsws))
      println("αDeg=" + αDeg + " -normalizedDotSquaredWithSign(o,a,b)=" + ( -ndsws) + " α=" + r2d(angle))
    }
}