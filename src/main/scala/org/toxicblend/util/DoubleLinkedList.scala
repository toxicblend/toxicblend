package org.toxicblend.util

import scala.collection.mutable.ArrayBuffer

class DoubleLinkedListElement[T] (var data:T, var p:DoubleLinkedListElement[T], var n:DoubleLinkedListElement[T] ) {
  protected[util] def delete = {
      if (p!=null) p.n = n
    if (n!=null) n.p = p
    n = null
    p = null
    this
  }
  
  protected[util] def insertAfter(element:DoubleLinkedListElement[T]) = {
    element.n = n
    element.p = this
    if (n !=null) n.p=element
    n = element
  }
  
  def hasNext = n != null
  def hasPrev = p != null
 
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
  var listSize = if (firstElement!=null) {
    var newListSize = 0
    var i = firstElement
    if (i!=null) {
      newListSize = 1
      while (i.hasNext) {
        newListSize+=1
        i=i.next
      }
      assert(i.eq(lastElement))
    }
    newListSize
  } else 0
  
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
    if (item>0){ 
      if(item >= listSize) throw new java.lang.ArrayIndexOutOfBoundsException 
    } else {
      if(item < -listSize) throw new java.lang.ArrayIndexOutOfBoundsException
    }
    if (item >= 0) (0 until item).foldLeft(firstElement) ((x,i) => x.n)
    else (1 until -item).foldLeft(lastElement) ((x,i) => x.p)
  }
  
  /**
   * cuts out the element from firstElement to lastElement from the list and creates a new list with the content
   */
  def cutSlice(e1:DoubleLinkedListElement[T], e2:DoubleLinkedListElement[T]):DoubleLinkedList[T] = {
    if (e1.eq(firstElement)) {
      firstElement = e2.n
    } else {
      e1.p.n = e2.n
    }
    if (e2.eq(lastElement)) {
      lastElement = e1.p
    } else {
      e2.n.p = e1.p
    }
    
    e1.p = null
    e2.n = null
      
    val rv = new DoubleLinkedList[T](e1, e2)
    listSize -= rv.size
    rv
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