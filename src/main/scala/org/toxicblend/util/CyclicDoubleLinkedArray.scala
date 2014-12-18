package org.toxicblend.util

class DoubleLinkedArrayElement[T](var value:T, var prev:Int, var next:Int)

class CyclicDoubleLinkedArray[T] private (val defaultValue:T, var indices:Array[DoubleLinkedArrayElement[T]] ) {
  
  def this(defaultValue:T, initialSize:Int) {
    this( defaultValue, new Array[DoubleLinkedArrayElement[T]](initialSize) )
    for (i<- 0 until initialSize) indices(i) = new DoubleLinkedArrayElement[T](defaultValue, i-1, i+1)
    setup(initialSize)
  }
  
  // an index to a vertex that has not been removed yet 
  protected var someplace = 0
  
  def getOne = someplace
  
  @inline def next(i:Int):Int = indices(i).next
  @inline def prev(i:Int):Int = indices(i).prev
  @inline def value(i:Int):T = indices(i).value
  
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
  
  /**
   * Resets the array so that it's size is inputSize and each element n has references to element n+1 and n-1
   */
  def setup(inputSize:Int) = {
    if (indices.size < inputSize) {
      // grow the container array
      val newIndices = new Array[DoubleLinkedArrayElement[T]](inputSize)
      val oldSize = indices.size
      for (i <- 0 until oldSize) {
        val e = indices(i)
        e.prev = (i-1 + inputSize) % inputSize
        e.next = (i+1 + inputSize) % inputSize
        newIndices(i) = e
      }
      for (i <- oldSize until inputSize) 
        newIndices(i) = new DoubleLinkedArrayElement[T](defaultValue, (i-1 + inputSize) % inputSize, (i+1 + inputSize) % inputSize)
      indices = newIndices
    } else {
      // no need to resize the container array
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