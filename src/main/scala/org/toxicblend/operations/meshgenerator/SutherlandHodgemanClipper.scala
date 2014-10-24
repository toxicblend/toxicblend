package org.toxicblend.operations.meshgenerator

import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec2D
import toxi.geom.Line2D

class SutherlandHodgemanClipper {
 
   def intersection(a:Vec2D, b:Vec2D, p:Vec2D, q:Vec2D) = {
    val A1 = b.y - a.y
    val B1 = a.x - b.x
    val C1 = A1 * a.x + B1 * a.y
    val A2 = q.y - p.y
    val B2 = p.x - q.x
    val C2 = A2 * p.x + B2 * p.y
    val det = A1 * B2 - A2 * B1
    new Vec2D((B2 * C1 - B1 * C2) / det, (A1 * C2 - A2 * C1) / det)
  }

  def isInside(p:Vec2D, a:Vec2D, b:Vec2D) = (a.x-p.x)*(b.y-p.y)>(a.y-p.y)*(b.x-p.x)
  
  def clipPolygon(input:IndexedSeq[Vec2D], a:Vec2D, b:Vec2D):IndexedSeq[Vec2D] = {
    (0 until input.size).foldLeft(new ArrayBuffer[Vec2D]) ((x,i) => {
      val current = input(i)
      val previous = input((i+input.size-1)%input.size)
      (isInside(current, a, b),isInside(previous, a, b)) match {
        case (true,true) => x += current
        case (true,false) => x += intersection(a,b,current,previous) += current
        case (false,true) => x += intersection(a,b,current,previous)
        case _ => 
      }
      x
    })
  }
  
  def clipPolygon(input:IndexedSeq[Vec2D], clipEdge:Line2D):IndexedSeq[Vec2D] = 
    clipPolygon(input, clipEdge.a, clipEdge.b)
    
  def clipPolygon(input:IndexedSeq[Vec2D], clipEdges:IndexedSeq[Vec2D]):IndexedSeq[Vec2D] = {
    val clipSize = clipEdges.size
    (0 until clipSize).foldLeft(input)((p,i) => clipPolygon(p, clipEdges((i+clipSize-1) % clipSize), clipEdges(i)))
  }
}

object SutherlandHodgemanClipper {
  val singleton = new SutherlandHodgemanClipper
}