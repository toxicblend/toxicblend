package org.toxicblend.tests

import org.scalatest._

import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.AABB2D
import org.toxicblend.vecmath.Polygon2D


import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class AABB2DTest extends VecMathBaseTest {
  
  "AABB2DTest-1" should "test intersects" in {
    val aabb1 = AABB2D(100,100,200,200)
    val aabb2 = AABB2D(120,120,180,180)
    
    aabb1.intersects(aabb2) should be (true)
    aabb2.intersects(aabb1) should be (true)
    aabb2.intersects(aabb2) should be (true)
    aabb1.intersects(aabb1) should be (true)
  }
  
  "AABB2DTest-2" should "test intersects" in {
    val aabb1 = AABB2D(100,100,200,200)
    val aabb2 = AABB2D(20,20,80,80)
    
    aabb1.intersects(aabb2) should be (false)
    aabb2.intersects(aabb1) should be (false)
    aabb2.intersects(aabb2) should be (true)
    aabb1.intersects(aabb1) should be (true)
  }
  
  "AABB2DTest-3" should "test toIndexedSeq" in {
    val aabb = AABB2D(100,100,200,200)
    val clockwise = aabb.toIndexedSeq(true)
    Polygon2D.isClockwise(clockwise) should be (true)
    Polygon2D(clockwise).isClockwise should be (true)
    val antiClockwise = aabb.toIndexedSeq(false)
    Polygon2D.isClockwise(antiClockwise) should be (false)
    Polygon2D(antiClockwise).isClockwise should be (false)
  }
  
}