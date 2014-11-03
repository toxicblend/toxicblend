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
  
  "Polygon2DTest-1" should "detect inside just fine" in {
    val s = Array((100.0,200.0),(158.77852522924732,180.90169943749476),
                  (195.10565162951536,130.90169943749476),(195.10565162951536,100.0),
                  (300.0,100.0),(300.0,300.0),(100.0,300.0)).map(v=>Vec2D(v._1, v._2))
    val p = new Polygon2D(s)
    p.containsPoint(Vec2D(100,100)) should be (false)
   }
}