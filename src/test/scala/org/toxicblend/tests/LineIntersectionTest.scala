package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.vecmath.ImmutableVec2D
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.FiniteLine2D
import org.toxicblend.vecmath.InfiniteLine2D
import org.toxicblend.vecmath.Polygon2D
import org.toxicblend.vecmath.SimpleIntersection
import Vec2DMatcher._

class LineIntersectionTest extends FlatSpec with Matchers {
  
  val doubleTolerance = 0.0001d
  
  /** 
   *  y = x
   *        /
   *       /
   * -----/-------
   *     /
   *    /
   */
  "LineIntersectionTest-1" should "find the intersection" in {
    val iLine = InfiniteLine2D(-.1d,-.1d, .1d,.1d)
    for( y<- -100 to 100) {
      val intersection = iLine.intersectLine(Vec2D(-100,y), Vec2D(100,y))
      intersection.isDefined should be (true)
      intersection.get match {
        case i:SimpleIntersection =>  i.p should equal2d (Vec2D(y,y),doubleTolerance)
        case _ => assert(false, "No SimpleIntersection was detected")
      }
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
  "LineIntersectionTest-2" should "find the intersection" in {
    val iLine = InfiniteLine2D(-.1d,.1f, .1d,-.1d)
    for( y<- -100 to 100) {
      val fromV = Vec2D(-100,y)
      val toV = Vec2D(100,y)
      val rv = InfiniteLine2D.intersectLine(iLine.a, iLine.b, fromV, toV)
      rv.isDefined should be (true)
      rv.get match {
        case i:SimpleIntersection =>  i.p should equal2d (Vec2D(-y,y),doubleTolerance)
        case _ => assert(false, "No SimpleIntersection was detected")
      }
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
  "LineIntersectionTest-3" should "find the intersection" in {
    val iLine = InfiniteLine2D(0f,.1d, 0d,-.1d)
    for( y<- -100 to 100) {
      val fromV = Vec2D(-10,y)
      val toV = Vec2D(10,y)
      val rv = InfiniteLine2D.intersectLine(iLine.a, iLine.b, fromV, toV)
      rv.isDefined should be (true)
      rv.get match {
        case i:SimpleIntersection =>  i.p should equal2d (Vec2D(0,y),doubleTolerance)
        case _ => assert(false, "No SimpleIntersection was detected")
      }
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
  "LineIntersectionTest-4" should "find the intersection" in {
    val iLine = InfiniteLine2D(0d,-.1d, 0d,.1d)
    for( y<- -100 to 100) {
      val fromV = Vec2D(-10,y)
      val toV = Vec2D(10,y)
      
      val rv = InfiniteLine2D.intersectLine(iLine.a, iLine.b, fromV, toV)
      rv.isDefined should be (true)
      rv.get match {
        case i:SimpleIntersection =>  i.p should equal2d (Vec2D(0,y),doubleTolerance)
        case _ => assert(false, "No SimpleIntersection was detected")
      }
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
    val iLine = FiniteLine2D(-.1d,-.1d, .1d,.1d)
    for( y<- -100 to 100) {
      val fromV = Vec2D(101,y)
      val toV = Vec2D(201,y)
      var rv = FiniteLine2D.intersectLine(iLine.a, iLine.b, fromV, toV)
      rv.isDefined should be (false)
      rv = iLine.intersectLine(fromV, toV)
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
    val iLine = FiniteLine2D(-.1d,-.1d,.1d,.1d)
    for( y<- -100 to 100) {
      val fromV = Vec2D(-101,y)
      val toV = Vec2D(-201,y)
      val rv = FiniteLine2D.intersectLine(iLine.a, iLine.b, fromV, toV)
      rv.isDefined should be (false)
    }
  }
  
  "LineIntersectionTest-7" should "test isClockwise" in {
    val p = Polygon2D(IndexedSeq((1,0),(0,1),(0,0)).map(v=>Vec2D(v._1,v._2)))
    p.isClockwise should be (false)
    p.isSelfIntersecting should be (false)
    p.getCentroid should be (Vec2D(1d/3d, 1d/3d))
    p.getArea should be (-0.5d plusOrMinus doubleTolerance)
  }
  
  "LineIntersectionTest-8" should "test isClockwise" in {
    val seq = collection.mutable.ArrayBuffer((1,0),(0,0),(0,1)).map(v=>Vec2D(v._1,v._2))
    (0 to 3).foreach(i => {
      //println(seq)
      val p = Polygon2D(seq)
      p.isClockwise should be (true)
      val v = seq.head
      seq.remove(0)
      seq.append(v)
    })
  }
  
  "LineIntersectionTest-9" should "test isClockwise" in {
    val seq = collection.mutable.ArrayBuffer((1,0),(0,1),(0,0)).map(v=>Vec2D(v._1,v._2))
    (0 to 3).foreach(i => {
      //println(seq)
      val p = Polygon2D(seq)
      p.isClockwise should be (false)
      val v = seq.head
      seq.remove(0)
      seq.append(v)
    })
  }
  
  "LineIntersectionTest-10" should "test isSelfIntersecting" in {
    val seq = collection.mutable.ArrayBuffer((0,0),(1,0),(0,1),(1,1)).map(v=>Vec2D(v._1,v._2))
    (0 to 5).foreach(i => {
      //println(seq)
      val p = Polygon2D(seq)
      p.isSelfIntersecting should be (true)
      val v = seq.head
      seq.remove(0)
      seq.append(v)
    })
  }
  
  "LineIntersectionTest-11" should "test isSelfIntersecting" in {
    val seq = collection.mutable.ArrayBuffer((0,0),(1,0),(1,1),(0,1)).map(v=>Vec2D(v._1,v._2))
    (0 to 5).foreach(i => {
      //println(seq)
      val p = Polygon2D(seq)
      p.isSelfIntersecting should be (false)
      val v = seq.head
      seq.remove(0)
      seq.append(v)
    })
  }
}