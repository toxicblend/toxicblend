package org.toxicblend.util

class SortedDoubleLinkedElement[T <% Double](var value:T, var prev:Int, var next:Int)

class SortedLinearDoubleLinkedArray[T <% Double] private (var indices:Array[SortedDoubleLinkedElement[T]] ) {
  
  def this(initialSize:Int=4, initialValue:T, setupAsEmpty:Boolean=true) {
    this( new Array[SortedDoubleLinkedElement[T]](initialSize) )
    for (i<- 0 until initialSize) indices(i) = new SortedDoubleLinkedElement[T](initialValue,i-1, i+1)    
    setup(initialSize, initialValue, setupAsEmpty=setupAsEmpty)
  }
  
  private var theHead = 0
  private var theLast = 0

  @inline def next(i:Int):Int = indices(i).next
  @inline def prev(i:Int):Int = indices(i).prev
  
  def foreach[U](f:(Int,SortedDoubleLinkedElement[T]) => U) = {
    var i = theHead
    while(i != -1) {
      val ie = indices(i)
      f(i,ie)
      i = ie.next  
    } 
  }
   
  def head:Int = theHead
  def last:Int = theLast
  
  /**
   * Don't call this method unless you keep track of 'theHead' and 'theLast'
   */
  @inline private def connect(i:Int, j:Int):Unit = {
    if ( i < 0 && j < 0 ) return    
    if (i != -1) indices(i).next = j
    if (j != -1) indices(j).prev = i
  }
  
  /**
   */
  @inline def contains(i:Int): Boolean = {
    if (isEmpty) return false
    val iE = indices(i)
    if (iE.next < 0 && iE.prev < 0 && theHead==i) return true
    iE.next >= 0 || iE.prev >= 0
  }
  
  @inline def isEmpty:Boolean = theHead == -1
  
  /**
   * you must be sure that the element you drop is actually part of the list
   */
  @inline def drop(i:Int):Unit = {
    assert(i>=0)
    val iE = indices(i)
    if (theHead == i) {
      assert(iE.prev<0)
      theHead = iE.next
    }
    if (theLast == i) {
      assert(iE.next < 0)
      theLast = iE.prev
    }
    connect(iE.prev, iE.next)
    
    iE.next = -1
    iE.prev = -1
  }
  
  @inline def safeDrop(i:Int) {
    if (contains(i)) {
      drop(i)
    }
  }
    
  /**
   * inserts a previous dropped element
   */
  @inline def add(i:Int, value:T) {
    val iE = indices(i)
    iE.value = value 
    assert(i>=0)
    if (theHead<0){
      // was an empty list, just add the element
      iE.next = -1
      iE.prev = -1
      theHead = i
      theLast = i
    } else {
      insertBefore(theHead, i)
      realUpdate(i,iE)
    }
  }
  
  @inline protected def insertBefore(pos:Int, i:Int) = {
    val iE = indices(i)
    if (iE.next != -1 || iE.prev != -1) drop(i)
    val prev = indices(pos).prev 
    connect(i,pos)
    if (prev<0) {
      theHead = i
    } else {
      connect(prev,i)
    }
  }
  
  @inline protected def insertAfter(pos:Int, i:Int) = {
    val iE = indices(i)
    if (iE.next != -1 || iE.prev != -1) drop(i)
    val next = indices(pos).next 
    connect(pos,i)
    if (next<0){
      theLast = i
    } else {
      connect(i,next)
    }
  }
  
  @inline def update(i:Int, value:T) {
    val iE = indices(i)
    if (iE.value != value) {
      iE.value = value
      realUpdate(i, iE)
    }
  }
  
  /**
   * move the element to the correct position
   */
  @inline protected def realUpdate(i:Int, iE:SortedDoubleLinkedElement[T]) { 
    if (iE.next != -1 && iE.value < indices(iE.next).value ) {
      // search next
      var ins = iE.next
      var insE = indices(ins)
      var prev = ins
      while(ins != -1 && iE.value < insE.value ){
        prev = ins
        ins = indices(ins).next
        if (ins>=0) insE = indices(ins)
      }
      drop(i)
      insertAfter(prev,i)
    } else if (iE.prev != -1 && iE.value >= indices(iE.prev).value ) {
      // search prev
      var ins = iE.prev
      var insE = indices(ins)
      var next = ins
      while(ins != -1 && iE.value >= insE.value ){
        next = ins
        ins = indices(ins).prev
        if (ins>=0) insE = indices(ins)
      }
      drop(i)
      insertBefore(next,i)
    }
  }
  
  @inline def safeAdd(i:Int, value:T) {
    if (!contains(i)) add(i, value)
    else if (indices(i).value != value) update(i, value)
  }
  
  def toIndexedSeq:IndexedSeq[Int] = {
    if (theHead != -1){
      var rv = new collection.mutable.ArrayBuffer[Int]

      var i = theHead
      rv.append(theHead)
      i = next(theHead)
      while (i != -1){
        rv.append(i)
        i = next(i)
      }
      rv
    } else Array[Int]()
  }
  
  def setup(inputSize:Int, initialValue:T, setupAsEmpty:Boolean=false) = {
    if (indices.size < inputSize) {
      val newIndices = new Array[SortedDoubleLinkedElement[T]](inputSize)
      val oldSize = indices.size
      for (i <- 0 until oldSize) {
        val e =  indices(i)
        if (setupAsEmpty){
          e.prev = -1
          e.next = -1
        } else {
          e.prev = i-1
          e.next = i+1
        }
        newIndices(i) = e
      }
      for (i <- oldSize until inputSize) 
        newIndices(i) = if (setupAsEmpty) new SortedDoubleLinkedElement(initialValue,-1, -1) else new SortedDoubleLinkedElement(initialValue,i-1, i+1) 
      indices = newIndices
    } else {
      for (i <- 0 until inputSize) {
        val e =  indices(i)
        if (setupAsEmpty){
          e.prev = -1
          e.next = -1
        } else {
          e.prev = i-1
          e.next = i+1
        }
      }
    }
    if (!setupAsEmpty){
      indices(0).prev = -1
      indices(inputSize-1).next = -1
      theHead = 0
      theLast = inputSize -1
    } else {
      theHead = -1
      theLast = -1
    }
  }
  @inline def apply(i:Int) = indices(i)
}

object Test extends App {
  val l = new SortedLinearDoubleLinkedArray[Double](4,0)
  l.add(0, 0d)
  l.foreach( (i,e) => println("#" + i + ": v=" + e.value + " p=" + e.prev + " n=" + e.next ))
  println("l.head=" + l.head)
  l.add(1, 1d)
  l.foreach( (i,e) => println("#" + i + ": v=" + e.value + " p=" + e.prev + " n=" + e.next ))
  println("l.head=" + l.head)
  l.add(2, 0.5d)
  l.foreach( (i,e) => println("#" + i + ": v=" + e.value + " p=" + e.prev + " n=" + e.next ))
  println("l.head=" + l.head)
  l.add(3, 1.5d)
  l.foreach( (i,e) => println("#" + i + ": v=" + e.value + " p=" + e.prev + " n=" + e.next ))
  println("l.head=" + l.head)
  l.safeDrop(1)
  l.foreach( (i,e) => println("#" + i + ": v=" + e.value + " p=" + e.prev + " n=" + e.next ))
  println("l.head=" + l.head)
  l.safeDrop(0)
  l.foreach( (i,e) => println("#" + i + ": v=" + e.value + " p=" + e.prev + " n=" + e.next ))
  println("l.head=" + l.head)

}