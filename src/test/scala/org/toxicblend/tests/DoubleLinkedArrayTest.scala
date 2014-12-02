package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.util.CyclicDoubleLinkedArray
import org.toxicblend.util.LinearDoubleLinkedArray
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class DoubleLinkedArrayTest extends VecMathBaseTest {
  
  val tolerance = 0.000001d
  
  "DoubleLinkedArrayTest-1" should "test setup with different sizes" in {
    var size = 10
    def next(i:Int) = if (i>=size-1) 0 else i+1
    def prev(i:Int) = if (i<=0) size-1 else i-1
    
    val vertices = new CyclicDoubleLinkedArray(size)
     
    for (i <- 0 until size){
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
    size = 20
    vertices.setup(size)
    for (i <- 0 until size){
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
    size = 10
    vertices.setup(size)
    for (i <- 0 until size){
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
  }
  
  "DoubleLinkedArrayTest-2" should "test drop" in {
    var size = 4
    val vertices = new CyclicDoubleLinkedArray(size)
    vertices.toIndexedSeq should contain inOrderOnly (0, 1, 2, 3) 
    vertices.drop(0)
    vertices.toIndexedSeq should contain inOrderOnly (3, 1, 2)
    vertices.setup(4)
    vertices.drop(3)
    vertices.toIndexedSeq should contain inOrderOnly (2, 0, 1)
    vertices.drop(1)
    vertices.toIndexedSeq should contain inOrderOnly (0, 2)
    vertices.drop(0)
    vertices.toIndexedSeq.size should be (1)
    vertices.toIndexedSeq(0) should be (2)
    
  }
  
  "DoubleLinkedArrayTest-3" should "test connect" in {
    var size = 5
    val vertices = new CyclicDoubleLinkedArray(size)
    vertices.connect(3,1) 
    vertices.toIndexedSeq should contain inOrderOnly (3,1,2)
    vertices.connect(1,3)
    vertices.toIndexedSeq should contain inOrderOnly (1,3)
    vertices.drop(3)
    vertices.toIndexedSeq.size should be (1)
    vertices.toIndexedSeq(0) should be (1)
    vertices.drop(1)
    vertices.toIndexedSeq.size should be (0)
  }
  
  "DoubleLinkedArrayTest-10" should "test setup with different sizes" in {
    var size = 10
    def next(i:Int) = if (i>=size-1) -1 else i+1
    def prev(i:Int) = if (i<=0) -1 else i-1
    
    val vertices = new LinearDoubleLinkedArray(size)
     
    for (i <- 0 until size){
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
    size = 20
    vertices.setup(size)
    for (i <- 0 until size){
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
    size = 10
    vertices.setup(size)
    for (i <- 0 until size){
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
  }
  
  "DoubleLinkedArrayTest-11" should "test drop" in {
    var size = 4
    val vertices = new LinearDoubleLinkedArray(size)
    vertices.toIndexedSeq should contain inOrderOnly (0,1,2,3) 
    vertices.drop(0)
    vertices.toIndexedSeq should contain inOrderOnly (1,2,3)
    vertices.head should be (1)
    
    vertices.setup(4)
    vertices.drop(3)
    vertices.toIndexedSeq should contain inOrderOnly (0,1,2)
    vertices.drop(1)
    vertices.toIndexedSeq should contain inOrderOnly (0,2)
    vertices.drop(0)
    vertices.toIndexedSeq.size should be (1)
    vertices.toIndexedSeq(0) should be (2)
    
  }
}

object DoubleLinkedArrayTest extends App {
  
  var size = 3
  val vertices = new LinearDoubleLinkedArray(size)
  vertices.setup(4)
  vertices.drop(3)
    
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev ) 
  }
  println("seq=" + vertices.toIndexedSeq.mkString(","))
  println
  
  println("drop(0)")
  vertices.drop(0)
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev )
  }
  println("seq=" + vertices.toIndexedSeq.mkString(","))
  println
  
  println("drop(2)")
  vertices.drop(2)
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev )
  }
  println("seq=" + vertices.toIndexedSeq.mkString(","))
  println
  println("drop(1)")
  vertices.drop(1)
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev )
  }
  println
  println("add(1)")
  vertices.add(1)
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev )
  }
  println("add(2)")
  vertices.add(2)
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev )
  }
  println("add(0)")
  vertices.add(0)
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev )
  }
  
  
  println("seq=" + vertices.toIndexedSeq.mkString(","))
  size = 20
  vertices.setup(size)
  size = 10
  vertices.setup(size)
  for (i <- 0 until size){
   println("i=" + i + " n=" + vertices(i).next + " p=" + vertices(i).prev )
  }
  println("seq=" + vertices.toIndexedSeq.mkString(","))
}