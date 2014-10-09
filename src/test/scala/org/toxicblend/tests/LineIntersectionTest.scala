package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.operations.meshgenerator.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import toxi.geom.Vec2D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Line2D

class LineIntersectionTest extends FlatSpec with Matchers {
  
  val floatTolerance = 0.0001f
  
  /** 
   *  y = x
   *        /
   *       /
   * -----/-------
   *     /
   *    /
   */
  "LineIntersectionTest-1" should "work just fine" in {
    val iLine = new Line2D(new Vec2D(-.1f,-.1f), new Vec2D(.1f,.1f))
    for( y<- -100 to 100) {
      val fromV = new Vec2D(-100,y)
      val toV = new Vec2D(100,y)
      val rv = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(iLine, fromV, toV)
      rv.isDefined should be (true)
      val rvv = rv.get
      rvv.x should be (y.toFloat plusOrMinus floatTolerance ) 
      rvv.y should be (y.toFloat plusOrMinus floatTolerance)
    }
  }
  
  
  /** 
   *  y = -x
   *    \  
   *     \ 
   * -----\-----
   *       \
   *        \
   */
  "LineIntersectionTest-2" should "work just fine" in {
    val iLine = new Line2D(new Vec2D(-.1f,.1f), new Vec2D(.1f,-.1f))
    for( y<- -100 to 100) {
      val fromV = new Vec2D(-100,y)
      val toV = new Vec2D(100,y)
      val rv = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(iLine, fromV, toV)
      rv.isDefined should be (true)
      val rvv = rv.get
      rvv.x should === (-y.toFloat plusOrMinus floatTolerance)
      rvv.y should === (y.toFloat plusOrMinus floatTolerance)
    }
  }
  
  /** 
   *  x = 0
   *    |  
   *    |  
   * ---|---
   *    |
   *    |
   */
  "LineIntersectionTest-3" should "work just fine" in {
    val iLine = new Line2D(new Vec2D(0f,.1f), new Vec2D(0f,-.1f))
    for( y<- -100 to 100) {
      val fromV = new Vec2D(-10,y)
      val toV = new Vec2D(10,y)
      val rv = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(iLine, fromV, toV)
      rv.isDefined should be (true)
      val rvv = rv.get
      rvv.x should === (0f plusOrMinus floatTolerance)
      rvv.y should === (y.toFloat plusOrMinus floatTolerance)
    }
  }
  
  /** 
   *  x = 0
   *    |  
   *    |  
   * ---|---
   *    |
   *    |
   */
  "LineIntersectionTest-4" should "work just fine" in {
    val iLine = new Line2D(new Vec2D(0f,-.1f), new Vec2D(0f,.1f))
    for( y<- -100 to 100) {
      val fromV = new Vec2D(-10,y)
      val toV = new Vec2D(10,y)
      
      val rv = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(iLine, fromV, toV)
      rv.isDefined should be (true)
      val rvv = rv.get
      rvv.x should === (0f plusOrMinus floatTolerance)
      rvv.y should === (y.toFloat plusOrMinus floatTolerance)
    }
  }
  
  /** 
   *  y = x
   *        / ----
   *       /
   * -----/-------
   *     /
   *    /
   */
  "LineIntersectionTest-5" should "not find any intersections" in {
    val iLine = new Line2D(new Vec2D(-.1f,-.1f), new Vec2D(.1f,.1f))
    for( y<- -100 to 100) {
      val fromV = new Vec2D(101,y)
      val toV = new Vec2D(201,y)
      val rv = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(iLine, fromV, toV)
      rv.isDefined should be (false)
    }
  }
  
    /** 
   *  y = x
   *  ----  / 
   *       /
   * -----/-------
   *     /
   *    /
   */
  "LineIntersectionTest-6" should "not find any intersections" in {
    val iLine = new Line2D(new Vec2D(-.1f,-.1f), new Vec2D(.1f,.1f))
    for( y<- -100 to 100) {
      val fromV = new Vec2D(-101,y)
      val toV = new Vec2D(-201,y)
      val rv = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(iLine, fromV, toV)
      rv.isDefined should be (false)
    }
  }
}