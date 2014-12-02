package org.toxicblend.util

class LinearDoubleLinkedArray private (var indices:Array[DoubleLinkedElement] ) {
  def this(initialSize:Int=4 ) {
    this( new Array[DoubleLinkedElement](initialSize) )
    for (i<- 0 until initialSize) indices(i) = new DoubleLinkedElement(i-1, i+1)    
    setup(initialSize)
  }
  
  // an index to a vertex that has not been removed yet 
  private var theHead = 0
  
  @inline def next(i:Int):Int = indices(i).next
  @inline def prev(i:Int):Int = indices(i).prev
  
  def head:Int = theHead
  
  /**
   * Don't call this method unless you keep track of 'theHead'
   */
  @inline private def connect(i:Int, j:Int):Unit = {
    if ( i < 0 && j < 0 ) {
      return
    }
      
    if (i != -1){
      val eI = indices(i)
      eI.next = j
      if (eI.prev != -1)
        indices(eI.prev).next = i
    }
    
    if (j != -1){    
      val eJ = indices(j)
      if (eJ.next != -1)
        indices(eJ.next).prev = j
      eJ.prev = i
    }
  }
  
  /**
   */
  def contains(element:Int): Boolean = {
    if (isEmpty) return false
    val e = indices(element)
    if (e.next < 0 && e.prev < 0 && theHead==element) return true
    e.next >= 0 || e.prev >= 0
  }
  
  @inline def isEmpty = theHead == -1
  
  /**
   * you must be sure that the element you drop is actually part of the list
   */
  @inline def drop(i:Int) {
    assert(i>=0)
    val ie = indices(i)
    if (theHead == i) {
      assert(ie.prev == -1)
      theHead = ie.next
    }
    connect(ie.prev, ie.next)
    
    ie.next = -1
    ie.prev = -1
  }
  
  @inline def safeDrop(i:Int) {
    if (contains(i)) {
      drop(i)
    }
  }
    
  /**
   * inserts a previous dropped element just ahead of 'theHead'
   */
  @inline def add(i:Int) {
    assert(i>=0)
    if (theHead<0){
      theHead = i
    } else {
      val ie = indices(i)
      val se = indices(theHead)
      if (se.prev >= 0) {
        val spe= indices(se.prev)
        spe.next = i
      } 
      ie.prev = se.prev
      se.prev = i
      ie.next = theHead
    }
    theHead = i
  }
  
  @inline def safeAdd(i:Int) {
    if (!contains(i)) {
      add(i)
    }
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
  
  def setup(inputSize:Int, empty:Boolean=false) = {
    if (indices.size < inputSize) {
      val newIndices = new Array[DoubleLinkedElement](inputSize)
      val oldSize = indices.size
      for (i <- 0 until oldSize) {
        val e =  indices(i)
        if (empty){
          e.prev = -1
          e.next = -1
        } else {
          e.prev = i-1
          e.next = i+1
        }
        newIndices(i) = e
      }
      for (i <- oldSize until inputSize) 
        newIndices(i) = if (empty) new DoubleLinkedElement(-1, -1) else new DoubleLinkedElement(i-1, i+1) 
      indices = newIndices
    } else {
      for (i <- 0 until inputSize) {
        val e =  indices(i)
        if (empty){
          e.prev = -1
          e.next = -1
        } else {
          e.prev = i-1
          e.next = i+1
        }
      }
    }
    if (!empty){
      indices(0).prev = -1
      indices(inputSize-1).next = -1
      theHead = 0
    } else {
      theHead = -1
    }
  }
  @inline def apply(i:Int) = indices(i)
}