package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.vecmath.SutherlandHodgemanClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.vecmath.ImmutableVec2D
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.FiniteLine2D
import org.toxicblend.vecmath.Polygon2D

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
  "LineIntersectionTest-1" should "work just fine" in {
    val iLine = new FiniteLine2D(new ImmutableVec2D(-.1f,-.1f), new ImmutableVec2D(.1f,.1f))
    for( y<- -100 to 100) {
      val fromV = new ImmutableVec2D(-100,y)
      val toV = new ImmutableVec2D(100,y)
      val rv = SutherlandHodgemanClipper.singleton.intersection(iLine.a, iLine.b, fromV, toV)
      rv.x should be (y.toDouble plusOrMinus doubleTolerance ) 
      rv.y should be (y.toDouble plusOrMinus doubleTolerance)
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
    val iLine = new FiniteLine2D(new ImmutableVec2D(-.1f,.1f), new ImmutableVec2D(.1f,-.1f))
    for( y<- -100 to 100) {
      val fromV = new ImmutableVec2D(-100,y)
      val toV = new ImmutableVec2D(100,y)
      val rv = SutherlandHodgemanClipper.singleton.intersection(iLine.a, iLine.b, fromV, toV)
      rv.x should === (-y.toDouble plusOrMinus doubleTolerance)
      rv.y should === (y.toDouble plusOrMinus doubleTolerance)
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
    val iLine = new FiniteLine2D(new ImmutableVec2D(0f,.1f), new ImmutableVec2D(0f,-.1f))
    for( y<- -100 to 100) {
      val fromV = new ImmutableVec2D(-10,y)
      val toV = new ImmutableVec2D(10,y)
      val rv = SutherlandHodgemanClipper.singleton.intersection(iLine.a, iLine.b, fromV, toV)
      rv.x should === (0d plusOrMinus doubleTolerance)
      rv.y should === (y.toDouble plusOrMinus doubleTolerance)
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
    val iLine = new FiniteLine2D(new ImmutableVec2D(0f,-.1f), new ImmutableVec2D(0f,.1f))
    for( y<- -100 to 100) {
      val fromV = new ImmutableVec2D(-10,y)
      val toV = new ImmutableVec2D(10,y)
      
      val rv = SutherlandHodgemanClipper.singleton.intersection(iLine.a, iLine.b, fromV, toV)
      rv.x should === (0d plusOrMinus doubleTolerance)
      rv.y should === (y.toDouble plusOrMinus doubleTolerance)
    }
  }
  
  /** 
   *  y = x
   *        / ----
   *       /
   * -----/-------
   *     /
   *    /
   *
  "LineIntersectionTest-5" should "not find any intersections" in {
    val iLine = new FiniteLine2D(Vec2D(-.1f,-.1f), Vec2D(.1f,.1f))
    for( y<- -100 to 100) {
      val fromV = Vec2D(101,y)
      val toV = Vec2D(201,y)
      val rv = SutherlandHodgemanClipper.singleton.intersection(iLine.a, iLine.b, fromV, toV)
      rv.x.isNaN should be (true)
      rv.y.isNaN should be (true)
    }
  }
  
  / ** 
   *  y = x
   *  ----  / 
   *       /
   * -----/-------
   *     /
   *    /
   *
  "LineIntersectionTest-6" should "not find any intersections" in {
    val iLine = new FiniteLine2D(Vec2D(-.1f,-.1f), Vec2D(.1f,.1f))
    for( y<- -100 to 100) {
      val fromV = Vec2D(-101,y)
      val toV = Vec2D(-201,y)
      val rv = SutherlandHodgemanClipper.singleton.intersection(iLine.a, iLine.b, fromV, toV)
      rv.x.isNaN should be (true)
      rv.y.isNaN should be (true)
    }
  }
  
  * */
  
  "LineIntersectionTest-7" should "test isClockwise" in {
    val p = Polygon2D(IndexedSeq((1,0),(0,1),(0,0)).map(v=>Vec2D(v._1,v._2)))
    p.isClockwise should be (false)
    p.isSelfIntersecting should be (false)
    p.getCentroid should be (Vec2D(1d/3d, 1d/3d))
    p.getArea should be (0.5d)
  }
  
  "LineIntersectionTest-8" should "test isClockwise" in {
    val seq = scala.collection.mutable.ArrayBuffer((1,0),(0,0),(0,1)).map(v=>Vec2D(v._1,v._2))
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
    val seq = scala.collection.mutable.ArrayBuffer((1,0),(0,1),(0,0)).map(v=>Vec2D(v._1,v._2))
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
    val seq = scala.collection.mutable.ArrayBuffer((0,0),(1,0),(0,1),(1,1)).map(v=>Vec2D(v._1,v._2))
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
    val seq = scala.collection.mutable.ArrayBuffer((0,0),(1,0),(1,1),(0,1)).map(v=>Vec2D(v._1,v._2))
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