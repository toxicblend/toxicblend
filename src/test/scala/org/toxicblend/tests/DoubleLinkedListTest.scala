package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.util.DoubleLinkedList

class DoubleLinkedListTest extends FlatSpec with Matchers {
  
  "DoubleLinkedListTest-1" should "test add and delete" in {
    val dll = new DoubleLinkedList[Int]
    dll.append(1)
    dll.append(2)
    dll.append(3)
    dll.append(4)
    dll.size should be (4)
    dll.delete(dll.last)
    dll.size should be (3)
    var i = dll.head
    i.data should be (1)
    i = i.next
    i.data should be (2)
    i = i.next
    i.data should be (3)
    i.hasNext should be (false)
    i = i.prev
    i.data should be (2)
    i = i.prev
    i.data should be (1)
    i.hasPrev should be (false)
    dll.delete(i)
    dll.size should be (2)
    i = dll.head
    i.data should be (2)
    i.hasPrev should be (false)
  }
  
  "DoubleLinkedListTest-2" should "test apply" in {
    val dll = new DoubleLinkedList[Int]
    dll.append(1)
    dll.append(2)
    dll.append(3)
    dll.append(4)
    var i = dll(1)
    i.data should be (2)
    i = dll(0)
    i.data should be (1)
    i = dll(-1)
    i.data should be (4)
    i = dll(-2)
    i.data should be (3)
    i = dll(-3)
    i.data should be (2)
    i = dll(-4)
    i.data should be (1)
  }
  
  "DoubleLinkedListTest-3" should "test cutSlice" in {
    val dll = new DoubleLinkedList[Int]
    dll.append(0)
    dll.append(1)
    dll.append(2)
    dll.append(3)
    dll.append(4)
    var i1 = dll(1)
    var i2 = dll(-2)
    val dll2 = dll.cutSlice(i1, i2)
    dll.size should be (2)
    dll2.size should be (3)
  }
  
  "DoubleLinkedListTest-4" should "test cutSlice" in {
    val dll = new DoubleLinkedList[Int]
    dll.append(0)
    dll.append(1)
    dll.append(2)
    dll.append(3)
    dll.append(4)
    var i1 = dll(0)
    var i2 = dll(-1)
    val dll2 = dll.cutSlice(i1, i2)
    dll.size should be (0)
    dll2.size should be (5)
    dll2(0).data  should be (0)
    dll2(1).data  should be (1)
    dll2(2).data  should be (2)
    dll2(3).data  should be (3)
    dll2(4).data  should be (4)
  }
  
  "DoubleLinkedListTest-5" should "test cutSlice" in {
    val dll = new DoubleLinkedList[Int]
    dll.append(0)
    dll.append(1)
    dll.append(2)
    dll.append(3)
    dll.append(4)
    var i1 = dll(2)
    var i2 = dll(-1)
    val dll2 = dll.cutSlice(i1, i2)
    dll.size should be (2)
    dll2.size should be (3)
    
    dll(0).data  should be (0)
    dll(1).data  should be (1)
    dll2(0).data  should be (2)
    dll2(1).data  should be (3)
    dll2(2).data  should be (4)
  }
  
  "DoubleLinkedListTest-6" should "test cutSlice" in {
    val dll = new DoubleLinkedList[Int]
    dll.append(0)
    dll.append(1)
    dll.append(2)
    dll.append(3)
    dll.append(4)
    var i1 = dll(0)
    var i2 = dll(-1)
    val dll2 = dll.cutSlice(i1, i2)
    dll.size should be (0)
    dll2.size should be (5)
    
    dll2(0).data  should be (0)
    dll2(1).data  should be (1)
    dll2(2).data  should be (2)
    dll2(3).data  should be (3)
    dll2(4).data  should be (4)
  }
}

object DoubleLinkedListTestApp extends App {
  val dll = new DoubleLinkedList[Int]
  dll.append(0)
  dll.append(1)
  dll.append(2)
  dll.append(3)
  dll.append(4)
  var i1 = dll(0)
  var i2 = dll(-1)
  val dll2 = dll.cutSlice(i1, i2)
  if (dll.size != 1110) 
    println("bug size=" + dll.size + " : " + dll.toIndexedSeq.map(e=>e.data).mkString(","))  
  if (dll2.size != 1115) 
    println("bug size=" + dll2.size + " : " + dll2.toIndexedSeq.map(e=>e.data).mkString(","))
  println("done")
}