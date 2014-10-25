package org.toxicblend.operations.meshgenerator.vecmath

import scala.collection.mutable.ArrayBuffer

class SutherlandHodgemanClipper {
 
   def intersection(a:Vertex2D, b:Vertex2D, p:Vertex2D, q:Vertex2D) = {
    val A1 = b.y - a.y
    val B1 = a.x - b.x
    val C1 = A1 * a.x + B1 * a.y
    val A2 = q.y - p.y
    val B2 = p.x - q.x
    val C2 = A2 * p.x + B2 * p.y
    val det = A1 * B2 - A2 * B1
    new ImmutableVertex2D((B2 * C1 - B1 * C2) / det, (A1 * C2 - A2 * C1) / det)
  }

  def isInside(p:Vertex2D, a:Vertex2D, b:Vertex2D) = (a.x-p.x)*(b.y-p.y)>(a.y-p.y)*(b.x-p.x)
  
  def clipPolygon(input:IndexedSeq[Vertex2D], a:Vertex2D, b:Vertex2D):IndexedSeq[Vertex2D] = {
    (0 until input.size).foldLeft(new ArrayBuffer[Vertex2D]) ((x,i) => {
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
  
  def clipPolygon(input:IndexedSeq[Vertex2D], clipEdge:Line2D):IndexedSeq[Vertex2D] = 
    clipPolygon(input, clipEdge.a, clipEdge.b)
    
  def clipPolygon(input:IndexedSeq[Vertex2D], clipEdges:IndexedSeq[Vertex2D]):IndexedSeq[Vertex2D] = {
    val clipSize = clipEdges.size
    (0 until clipSize).foldLeft(input)((p,i) => clipPolygon(p, clipEdges((i+clipSize-1) % clipSize), clipEdges(i)))
  }
}

object SutherlandHodgemanClipper {
  val singleton = new SutherlandHodgemanClipper
}