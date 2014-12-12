package org.toxicblend.vecmath

import scala.collection.mutable.ArrayBuffer

abstract class SutherlandHodgemanClipperBase(protected val bufferA:MutableVec2DArray, protected val bufferB:MutableVec2DArray, val ε:Double) {
  protected final def intersection(a:Vec2D, b:Vec2D, p:Vec2D, q:Vec2D) = {
    val a1 = b.y - a.y
    val b1 = a.x - b.x
    val c1 = a1 * a.x + b1 * a.y
    val a2 = q.y - p.y
    val b2 = p.x - q.x
    val c2 = a2 * p.x + b2 * p.y
    val det = a1 * b2 - a2 * b1
    Vec2D((b2 * c1 - b1 * c2) / det, (a1 * c2 - a2 * c1) / det)
  }
}

class SutherlandHodgemanClipper protected (a:MutableVec2DArray, b:MutableVec2DArray, ε:Double) 
  extends SutherlandHodgemanClipperBase(a,b, ε) {
 
  def this(initialSize:Int,ε:Double) = this(new MutableVec2DArray(initialSize), new MutableVec2DArray(initialSize), ε)
 
  private final def clipPolygon(input:MutableVec2DArray, a:Vec2D, b:Vec2D):MutableVec2DArray = {
    
    @inline def isInside(p:Vec2D) = (a.x-p.x)*(b.y-p.y) > (a.y-p.y)*(b.x-p.x)
    
    val size = input.size
    var previousI = size-1
    var rv = if (input.eq(bufferA)) bufferB.clear else bufferA.clear
    for ( i <- 0 until size) {
      val current = input(i)
      val previous = input(previousI)
      (isInside(current),isInside(previous)) match {
        case (true,true)  => rv.conditionalAppend(current,ε)
        case (true,false) => rv.conditionalAppend(intersection(a,b,current,previous),ε).conditionalAppend(current,ε)
        case (false,true) => rv.conditionalAppend(intersection(a,b,current,previous),ε)
        case _ => 
      }
      previousI = i
    }
    if ( rv.size >1 && rv.last.=~=(rv.head,ε)) rv.removeLast
    rv
  }
    
  def clip(input:IndexedSeq[Vec2D], clipEdges:IndexedSeq[Vec2D]):IndexedSeq[Vec2D] = {
    val clipSize = clipEdges.size
    var buffer = bufferA.copy(input)
    if (clipSize==2) { 
      buffer = clipPolygon(buffer, clipEdges.head, clipEdges.last)
    } else {
      var previousI = clipSize-1
      for ( i <- 0 until clipSize) {
        buffer = clipPolygon(buffer, clipEdges(previousI), clipEdges(i))
        previousI = i
      }
    }
    buffer.clone
  }
  
  def clip(input:Polygon2D, clipEdges:Polygon2D, ε:Double):Polygon2D = 
    Polygon2D(clip(input.vertices, clipEdges.vertices))
}

class SutherlandHodgemanRectangularClipper protected (a:MutableVec2DArray, b:MutableVec2DArray, ε:Double) 
  extends SutherlandHodgemanClipperBase(a,b,ε) {
  
  def this(initialSize:Int,ε:Double) = this(new MutableVec2DArray(initialSize), new MutableVec2DArray(initialSize), ε)

  private final def clip(input:MutableVec2DArray, a:Vec2D, b:Vec2D, isInside:(Vec2D) => Boolean):MutableVec2DArray = {
       
    val size = input.size
    var previousI = size-1
    var rv = if (input.eq(bufferA)) bufferB.clear else bufferA.clear

    for (i <- 0 until size) {
      val current = input(i)
      val previous = input(previousI)
      (isInside(current),isInside(previous)) match {
        case (true,true)  => rv.conditionalAppend(current,ε)
        case (true,false) => rv.conditionalAppend(intersection(a,b,current,previous),ε).conditionalAppend(current,ε)
        case (false,true) => rv.conditionalAppend(intersection(a,b,current,previous),ε)
        case _ => 
      }
      previousI = i
    }
    if ( rv.size >1 && rv.last.=~=(rv.head,ε)) rv.removeLast
    rv
  }
    
  def clip(input:IndexedSeq[Vec2D], aabb:AABB2D):IndexedSeq[Vec2D] = {
    val clipEdges = IndexedSeq(aabb.max, Vec2D(aabb.max.x, aabb.min.y), aabb.min, Vec2D(aabb.min.x, aabb.max.y))
    var buffer = bufferA.copy(input) 
    buffer = clip(buffer, clipEdges(3), clipEdges(0), (p:Vec2D)=>{p.y < aabb.max.y})
    buffer = clip(buffer, clipEdges(0), clipEdges(1), (p:Vec2D)=>{p.x < aabb.max.x})
    buffer = clip(buffer, clipEdges(1), clipEdges(2), (p:Vec2D)=>{p.y > aabb.min.y})
    buffer = clip(buffer, clipEdges(2), clipEdges(3), (p:Vec2D)=>{p.x > aabb.min.x})
    buffer.clone
  }
  
  def clip(input:Polygon2D, aabb:AABB2D):Polygon2D = {
    Polygon2D(clip(input.vertices, aabb))
  }
}

