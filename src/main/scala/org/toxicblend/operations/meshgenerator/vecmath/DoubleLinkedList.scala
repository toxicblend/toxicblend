package org.toxicblend.operations.meshgenerator.vecmath

import scala.collection.mutable.ArrayBuffer

class DoubleLinkedListElement[T] (val data:T, var p:DoubleLinkedListElement[T], var n:DoubleLinkedListElement[T] ) {
  protected[vecmath] def delete = {
      if (p!=null) p.n = n
    if (n!=null) n.p = p
    n = null
    p = null
    this
  }
  
  protected[vecmath] def insertAfter(element:DoubleLinkedListElement[T]) = {
    element.n = n
    element.p = this
    if (n !=null) n.p=element
    n = element
  }
  
  def hasNext = n != null
  def next = n
  def prev = p
  
  override def toString = {
    val pdata = if (p==null) "null" else p.data 
    val ndata = if (n==null) "null" else n.data
    "" + data.toString + " p:" + pdata + " n:" + ndata
  }
}

class DoubleLinkedListElementIterator[T] (private var alpha:DoubleLinkedListElement[T] ) extends Iterator[DoubleLinkedListElement[T]] {
  private var current = alpha
  override def hasNext = current != null && current.n != null
  
  override def next = {
    if (alpha!=null) {
      val rv = alpha
      alpha = null
      rv
    } else {
      current = current.n
      current
    }
  }
}

class DoubleLinkedList[T] (var firstElement:DoubleLinkedListElement[T]=null, var lastElement:DoubleLinkedListElement[T]=null) extends Iterable[DoubleLinkedListElement[T]]{
  
  override def head = firstElement
  override def last = lastElement
  var listSize = 0
  
  def delete(element:DoubleLinkedListElement[T]) = {
    if (firstElement eq element) {
      firstElement = element.n  
    }
    if (lastElement eq element) {
      lastElement = element.p  
    }
    element.delete
    listSize -= 1
  }
  
  def prepend(data:T) = { 
    val element:DoubleLinkedListElement[T] = new DoubleLinkedListElement[T](data, null, firstElement)
    if (firstElement!=null) firstElement.p = element
    else {
      assert(lastElement==null)
      lastElement = element
    }
    firstElement = element
    listSize += 1
    this
  }
  
  def append(data:T) = { 
    val element:DoubleLinkedListElement[T] = new DoubleLinkedListElement[T](data, lastElement, null)
    if (lastElement!=null) lastElement.n = element
    else {
      assert(firstElement==null)
      firstElement = element
    }
    lastElement = element
    listSize += 1
    this
  }
  
  def insertAfter(existingElement:DoubleLinkedListElement[T], data:T) = {
    val element = new DoubleLinkedListElement[T](data, null, null)
    if (lastElement!=null && lastElement.eq(existingElement)) lastElement = element
    existingElement.insertAfter(element)
    listSize += 1
    element
  }
  
  def apply(item:Int) = {
    if (item >= 0) (0 until item).foldLeft(firstElement) ((x,i) => x.n)
    else (1 until -item).foldLeft(lastElement) ((x,i) => x.p)
  }
  
  override def iterator = new DoubleLinkedListElementIterator(firstElement)
  
  def toArray:IndexedSeq[T] = {
    val rv = new ArrayBuffer[T]
    this.foreach( v => rv.append(v.data))
    rv.toIndexedSeq
  }
  
  /**
   * makes the first element the last
   */
  def moveHeadToLast = {
    val element = firstElement
    firstElement = element.n
    firstElement.p = null
    lastElement.n = element
    element.p = lastElement
    lastElement = element
    element.n = null
  }
  
  override def size = listSize
}

object Test extends App {
  val dll = new DoubleLinkedList[String]
  dll.append("2")
  dll.append("3")
  dll.prepend("1")
  dll.append("4")
  
  println("dll.first=" + dll.firstElement)
  println("dll.first.n=" + dll.firstElement.n)
  println("dll.first.n.n=" + dll.firstElement.n.n)
  
  println("dll.last=" + dll.lastElement)
  println("dll.last.p=" + dll.lastElement.p)
  println("dll.last.p.p=" + dll.lastElement.p.p)
  
  dll.delete(dll(3))
  println("dll.first=" + dll.firstElement)
  println("dll.first.n=" + dll.firstElement.n)
  println("dll.first.n.n=" + dll.firstElement.n.n)
  
  println("dll.last=" + dll.lastElement)
  println("dll.last.p=" + dll.lastElement.p)
  println("dll.last.p.p=" + dll.lastElement.p.p)
  
  println("0:" + dll(0))
  println("1:" + dll(1))
  println("2:" + dll(2))
  println("3:" + dll(3))
  
  println("-0:" + dll(-0))
  println("-1:" + dll(-1))
  println("-2:" + dll(-2))
  println("-3:" + dll(-3))
  
  println("firstElement:" + dll.firstElement )
  println("lastElement:" + dll.lastElement  )
  
  println("dll.mkString=" + dll.mkString( "(","),(",")" ))
  dll.moveHeadToLast
  println("moveHeadToLast=>" + dll.mkString( "(","),(",")" ))
  dll.moveHeadToLast
  println("moveHeadToLast=>" + dll.mkString( "(","),(",")" ))
  dll.moveHeadToLast
  println("moveHeadToLast=>" + dll.mkString( "(","),(",")" ))

}