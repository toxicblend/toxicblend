package org.toxicblend.vecmath

class MutableVec2DArray private (protected var vertices:Array[MutableVec2D]) extends Traversable[Vec2D]{
  protected var virtualSize = 0
  
  def this (intialSize:Int = 10) = {
    this (new Array[MutableVec2D](intialSize))
    assert(intialSize > 0)
    for (i <- 0 until intialSize) {
      vertices(i) = new MutableVec2D(0,0)
    }
  }
  
  def copy(input:IndexedSeq[Vec2D]):MutableVec2DArray = {
    val size = input.size
    ensureCapacity(size)
    for (i <- 0 until size) vertices(i).set(input(i))
    virtualSize = size
    this
  }
  
  def ensureCapacity(newSize:Int) = {
    if (vertices.size < newSize) {
      val newVertices = new Array[MutableVec2D](newSize)
      val oldSize = vertices.size
      for (i <- 0 until oldSize) {
        val e =  vertices(i)
        newVertices(i) = e
      }
      for (i <- oldSize until newSize) 
        newVertices(i) = new MutableVec2D(0,0)
      vertices = newVertices
    }
  }
  
  def clear = { 
    virtualSize = 0
    this
  }
  override def foreach[U](f:Vec2D => U) = vertices.foreach(f)
  
  def append(v:Vec2D):MutableVec2DArray = {
    if (virtualSize >= vertices.size) {
      ensureCapacity(vertices.size*2)
    } 
    vertices(virtualSize).set(v)      
    virtualSize += 1
    this
  }
  
  /**
   * append this vertex if it is substantially different from the last element 
   */
  def conditionalAppend(v:Vec2D, ε:Double) = {
    if (virtualSize==0 || ! last.=~=(v,ε) ) append(v)
    this
  }
  
  @inline def apply(i:Int) = vertices(i)
  override def size = virtualSize
  
  /**
   * deep copy everything into a new seq
   */
  override def clone:IndexedSeq[Vec2D] = {
    val rv = new Array[Vec2D](virtualSize)
    for (i <- 0 until virtualSize) 
      rv(i) = Vec2D(vertices(i))
    rv
  }
  
  override def last: MutableVec2D = if (virtualSize > 0) this(virtualSize - 1) else throw new NoSuchElementException
  override def head: MutableVec2D = if (virtualSize > 0) this(0) else throw new NoSuchElementException
  def removeLast = { 
    if (virtualSize > 0) 
      virtualSize-=1
    this
  }
}