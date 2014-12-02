package org.toxicblend.util

class DoubleLinkedElement(var prev:Int, var next:Int)

class CyclicDoubleLinkedArray private (var indices:Array[DoubleLinkedElement] ) {
  def this(initialSize:Int=4 ) {
    this( new Array[DoubleLinkedElement](initialSize) )
    for (i<- 0 until initialSize) indices(i) = new DoubleLinkedElement(i-1, i+1)
    setup(initialSize)
  }
  
  // an index to a vertex that has not been removed yet 
  protected var someplace = 0
  
  def getOne = someplace
  
  @inline def next(i:Int):Int = indices(i).next
  @inline def prev(i:Int):Int = indices(i).prev
  /**
   * the ordering of i and j matter
   */
  @inline def connect(i:Int, j:Int, disableRemoved:Boolean=true) = {
    if (disableRemoved){
      if (i < j) {
        // search with next
        var iC = indices(i).next 
        val jC = indices(i).prev
        while(iC!=jC) {
          val e = indices(iC)
          iC = e.next 
          e.next = -1
          e.prev = -1
        }
      } else if (i < j) {
        // search with prev
        var iC = indices(i).prev 
        val jC = indices(i).next
        while(iC!=jC) {
          val e = indices(iC)
          iC = e.prev 
          e.next = -1
          e.prev = -1
        }
      }
    }
    assert(i != -1 && j != -1 )
    //println("connecting " + i + " with " + j)
    val eI = indices(i)
    eI.next = j
    indices(eI.prev).next = i    
    val eJ = indices(j)
    indices(eJ.next).prev = j
    eJ.prev = i
    someplace = i
  }
  
  @inline def isEmpty = someplace == -1
  
  /**
   */
  def contains(element:Int): Boolean = {
    if (isEmpty) return false
    val e = indices(element)
    e.next >= 0 || e.prev >= 0
  }
  
  @inline def drop(i:Int) {
    val ie = indices(i)
    if (ie.prev == i || ie.next == i) {
      assert(ie.next==i && ie.prev==i)
      someplace = -1
    } else {
      connect(ie.prev, ie.next, false)
    }
    ie.next = -1
    ie.prev = -1
  }
  
  def toIndexedSeq:IndexedSeq[Int] = {
    if (someplace != -1){
      val rv = new collection.mutable.ArrayBuffer[Int]
      var c = 1
      var i = someplace
      rv.append(someplace)
      i = next(someplace)
      while (i!=someplace){
        rv.append(i)
        i = next(i)
        c += 1
        assert(c<=indices.size)
      }
      rv
    } else Array[Int]()
  }
  
  def setup(inputSize:Int) = {
    if (indices.size < inputSize) {
      val newIndices = new Array[DoubleLinkedElement](inputSize)
      val oldSize = indices.size
      for (i <- 0 until oldSize) {
        val e =  indices(i)
        e.prev = (i-1 + inputSize) % inputSize
        e.next = (i+1 + inputSize) % inputSize
        newIndices(i) = e
      }
      for (i <- oldSize until inputSize) 
        newIndices(i) = new DoubleLinkedElement((i-1 + inputSize) % inputSize, (i+1 + inputSize) % inputSize)
      indices = newIndices
    } else {
      for (i <- 0 until inputSize) {
        val e =  indices(i)
        e.prev = (i-1 + inputSize) % inputSize
        e.next = (i+1 + inputSize) % inputSize
      }
    }
    someplace = 0
  }
  
  @inline def apply(i:Int) = indices(i)
}