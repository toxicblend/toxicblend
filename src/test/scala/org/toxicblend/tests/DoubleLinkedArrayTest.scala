package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.util.CyclicDoubleLinkedArray
import org.toxicblend.util.LinearDoubleLinkedArray
import org.toxicblend.util.SortedLinearDoubleLinkedArray

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class DoubleLinkedArrayTest extends VecMathBaseTest {
  
  val tolerance = 0.000001d
  
  "DoubleLinkedArrayTest-1" should "test setup with different sizes" in {
    var size = 10
    def next(i:Int) = if (i>=size-1) 0 else i+1
    def prev(i:Int) = if (i<=0) size-1 else i-1
    
    val sillyDefaultValue = 1234
    val vertices = new CyclicDoubleLinkedArray(sillyDefaultValue, size)
     
    for (i <- 0 until size){
      vertices(i).value should be (sillyDefaultValue)
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
    size = 20
    vertices.setup(size)
    for (i <- 0 until size){
      vertices(i).value should be (sillyDefaultValue)
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
    size = 10
    vertices.setup(size)
    for (i <- 0 until size){
      vertices(i).value should be (sillyDefaultValue)
      vertices(i).next should be (next(i))
      vertices(i).prev should be (prev(i))
    }
  }
  
  "DoubleLinkedArrayTest-2" should "test drop" in {
    var size = 4
    val vertices = new CyclicDoubleLinkedArray(Unit, size)
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
    val vertices = new CyclicDoubleLinkedArray(1234,size)
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
    val sillyDefaultValue = 1234
    val vertices = new LinearDoubleLinkedArray(sillyDefaultValue, size, setupAsEmpty=false)
     
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
    val sillyDefaultValue = 1234
    val vertices = new LinearDoubleLinkedArray(sillyDefaultValue, size, setupAsEmpty=false)
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
  
  
  "DoubleLinkedArrayTest-20" should "test add and drop" in {
    val data = Array(0d,1d,2d,10d,3d,4,5d,0.5d,2.5d,6.5d)
    var size = data.size
    val l = new SortedLinearDoubleLinkedArray[Double](size,0,true)
    
    for (i <- 0 until data.size) {
      l.add(i,data(i))
    }
    l.toIndexedSeq.map(i=>l(i).value) should be (data.sortWith(_ > _).toSeq)
    
    l.drop(9)
    var data1 = data.dropRight(1)
    l.toIndexedSeq.map(i=>l(i).value) should be (data1.sortWith(_ > _).toSeq)
    
    l.drop(0)
    data1 = data1.drop(1)
    l.toIndexedSeq.map(i=>l(i).value) should be (data1.sortWith(_ > _).toSeq)
    
    data(0)=121
    l.add(0,data(0))
    data(9)=122
    l.add(9,data(9))
    l.toIndexedSeq.map(i=>l(i).value) should be (data.sortWith(_ > _).toSeq)
    
    data(0)=1223
    l.update(0,data(0))
    data(9)= -45d
    l.update(9,data(9))
    l.toIndexedSeq.map(i=>l(i).value) should be (data.sortWith(_ > _).toSeq)
    l.head should be (0)
    l.last should be (9)
  }
}
