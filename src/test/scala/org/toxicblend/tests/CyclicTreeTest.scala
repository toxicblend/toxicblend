package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.operations.meshgenerator.CyclicTree
import org.toxicblend.operations.meshgenerator.Payload
import org.toxicblend.ToxicblendException

class CyclicTreeTest extends FlatSpec with Matchers {
  "CyclicTreeTest-1" should "work just fine" in {
    val data = Array(Payload(0d),Payload(1d),Payload(2d),Payload(3d),Payload(4d),Payload(5d),Payload(6d),Payload(7d),Payload(8d))
    val tree = CyclicTree.apply(data)
    for ( x <- 1 until 8 ) {
      val rv = tree.searchIntervalWithLimits(0.5+x)
      rv.isDefined should be (true)
      rv.get._1.angle should be (0.0 + x)
      rv.get._2.angle should be (1.0 + x)
    }
    
    var rv = tree.searchIntervalWithLimits(8.5)
    rv.isDefined should be (true)
    rv.get._1.angle should be ( 8.0 )
    rv.get._2.angle should be ( 0.0 )
    
    tree.searchIntervalWithLimits(-1.5)
    rv.isDefined should be (true)
    rv.get._1.angle should be ( 8.0 )
    rv.get._2.angle should be ( 0.0 )
  }
  
  "CyclicTreeTest-2" should "work just fine" in {
    val data = Array(Payload(0d)).drop(1)
    val tree = CyclicTree.apply(data)
    
    val rv = tree.searchIntervalWithLimits(8.5)
    rv.isDefined should be (false)
  }
  
  
  "CyclicTreeTest-3" should "work just fine" in {
    val unordered = Array(Payload(4d),Payload(5d),Payload(6d),Payload(7d),Payload(8d),Payload(0d),Payload(1d),Payload(2d),Payload(3d))
    val order = CyclicTree.inOrder(unordered)
    
    for (i <- 0 until order.size) {
      order(i).angle.toInt should be (i)
    }
  }
  
  "CyclicTreeTest-4" should "generate an exception" in {
    val unordered = Array(Payload(1d),Payload(0d),Payload(3d))
    an [ToxicblendException] should be thrownBy CyclicTree.inOrder(unordered)
    
  }
}