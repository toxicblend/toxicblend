package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.operations.meshgenerator.vecmath.ImmutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.Payload
import org.toxicblend.operations.meshgenerator.vecmath.Vec2D
import org.toxicblend.operations.meshgenerator.vecmath.CyclicTree
import org.toxicblend.operations.meshgenerator.vecmath.Polygon2D
import org.toxicblend.ToxicblendException

class CyclicTreeTest extends FlatSpec with Matchers {
  
  "CyclicTreeTest-1" should "work just fine" in {
    val data = Array(5d,6d,7d,8d,-1d,0d,1d,2d,3d,4d).map(p=>Payload(p))
    val tree = CyclicTree(data, Vec2D(), Option(false))
    for ( x <- 0 until 8 ) {
      val rv = tree.searchIntervalWithLimits(0.5+x.toDouble)
      //println("searching for " + (0.5+x.toDouble) + " gave " + rv.get._1.angle + " to " + rv.get._2.angle)
      rv.isDefined should be (true)
      rv.get._1.angle should be (0.0 + x)
      rv.get._2.angle should be (1.0 + x)
    }
    
    var rv = tree.searchIntervalWithLimits(8.5)
    rv.isDefined should be (true)
    rv.get._1.angle should be ( 8.0 )
    rv.get._2.angle should be ( -1.0 )
    
    tree.searchIntervalWithLimits(-1.5)
    rv.isDefined should be (true)
    rv.get._1.angle should be ( 8.0 )
    rv.get._2.angle should be ( -1.0 )
  }
  
  "CyclicTreeTest-2" should "work just fine" in {
    val data = Array(Payload(0d)).drop(1)
    val tree = CyclicTree(data, Vec2D())
    
    val rv = tree.searchIntervalWithLimits(8.5)
    rv.isDefined should be (false)
  }
  
  
  "CyclicTreeTest-3" should "work just fine" in {
    val unordered = Array(4d,5d,6d,7d,8d,0d,1d,2d,3d).map(p=>Payload(p))
    val (order,clockwise) = CyclicTree.inOrder(unordered, Option(false))
    
    for (i <- 0 until order.size) {
      order(i).angle.toInt should be (i)
    }
  }
  
  "CyclicTreeTest-4" should "generate an exception" in {
    val unordered = Array(Payload(1d),Payload(0d),Payload(3d), Payload(4d))
    an [ToxicblendException] should be thrownBy CyclicTree.inOrder(unordered,  Option(true))
  }
  
  "CyclicTreeTest-5" should "not generate an exception" in {
    //println("CyclicTreeTest-5 start")
    val unordered = Array(Payload(1d),Payload(0d),Payload(3d))
    CyclicTree.inOrder(unordered, Option(true))
    //an [ToxicblendException] should not be thrownBy CyclicTree.inOrder(unordered)
    //println("CyclicTreeTest-5 end")
  }
  
  "CyclicTreeTest-6" should "work just fine" in {
    //println("CyclicTreeTest-6 start")
    val unordered = Array(1d,0d,8d,7d,6d,5d,4d,3d,2d).map(p=>Payload(p))
    val (order,clockwise) = CyclicTree.inOrder(unordered, Option(true))
    
    for (i <- 0 until order.size) {
      order(i).angle.toInt should be (order.size-i-1)
    }
    //println("CyclicTreeTest-6 end")
  }
  
  "CyclicTreeTest-7" should "work just fine" in {
    val data = Array(4d,5d,6d,7d,8d,-1d, 0d,1d,2d,3d).reverse.map(p=>Payload(p))
    val tree = CyclicTree(data, Vec2D(), Option(true))
    for ( x <- 0 until 8 ) {
      val rv1 = tree.searchIntervalWithLimits(0.5+x)
      rv1.isDefined should be (true)
      rv1.get._1.angle should be (0.0 + x)
      rv1.get._2.angle should be (1.0 + x)
      val rv2 = tree.searchIntervalWithLimits(0.1+x)
      rv2.isDefined should be (true)
      rv2.get._1.angle should be (0.0 + x)
      rv2.get._2.angle should be (1.0 + x)
    }
    
    var rv = tree.searchIntervalWithLimits(8.5)
    rv.isDefined should be (true)
    rv.get._1.angle should be ( -1.0 )
    rv.get._2.angle should be ( 8.0 )
    
    tree.searchIntervalWithLimits(-1.5)
    rv.isDefined should be (true)
    rv.get._1.angle should be ( -1.0 )
    rv.get._2.angle should be ( 8.0 )
  }
}