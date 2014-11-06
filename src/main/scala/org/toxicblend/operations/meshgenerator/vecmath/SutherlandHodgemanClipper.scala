package org.toxicblend.operations.meshgenerator.vecmath

import scala.collection.mutable.ArrayBuffer

class SutherlandHodgemanClipper {
 
   def intersection(a:Vec2D, b:Vec2D, p:Vec2D, q:Vec2D) = {
    val A1 = b.y - a.y
    val B1 = a.x - b.x
    val C1 = A1 * a.x + B1 * a.y
    val A2 = q.y - p.y
    val B2 = p.x - q.x
    val C2 = A2 * p.x + B2 * p.y
    val det = A1 * B2 - A2 * B1
    new ImmutableVec2D((B2 * C1 - B1 * C2) / det, (A1 * C2 - A2 * C1) / det)
  }

  def isInside(p:Vec2D, a:Vec2D, b:Vec2D) = (a.x-p.x)*(b.y-p.y)>(a.y-p.y)*(b.x-p.x)
  
  def clipPolygon(input:IndexedSeq[Vec2D], a:Vec2D, b:Vec2D, ε:Double):IndexedSeq[Vec2D] = {
    
    /*def conditionalAppend2(buffer:ArrayBuffer[Vec2D], v:Vec2D) = {
      buffer.append(v)
      buffer
    }*/
    def conditionalAppend(buffer:ArrayBuffer[Vec2D], v:Vec2D) = {
      if (buffer.size==0 || !buffer.last.=~=(v,ε) ) buffer.append(v)
      buffer
    } 
    
    var previousI = input.size-1
    //println("Clipping : " + a + " -> " + b )
    val rv = (0 until input.size).foldLeft(new ArrayBuffer[Vec2D]) ((x,i) => {
      val current = input(i)
      val previous = input(previousI)//input((i+input.size-1)%input.size)
      (isInside(current, a, b),isInside(previous, a, b)) match {
        case (true,true) => conditionalAppend(x,current)
        case (true,false) => conditionalAppend(conditionalAppend(x,intersection(a,b,current,previous)),current)
        case (false,true) => conditionalAppend(x,intersection(a,b,current,previous))
        case _ => 
      }
      previousI = i
      x
    })
    if ( rv.size >1 && rv.last.=~=(rv.head,ε)) rv.remove(rv.size-1)
    rv
  }
  
  def clipPolygon(input:IndexedSeq[Vec2D], clipEdge:FiniteLine2D, ε:Double):IndexedSeq[Vec2D] = 
    clipPolygon(input, clipEdge.a, clipEdge.b, ε)
    
  def clipPolygon(input:IndexedSeq[Vec2D], clipEdges:IndexedSeq[Vec2D], ε:Double):IndexedSeq[Vec2D] = {
    val clipSize = clipEdges.size
    if (clipSize==2) clipPolygon(input, clipEdges.head, clipEdges.last, ε)
    else (0 until clipSize).foldLeft(input)((p,i) => clipPolygon(p, clipEdges((i+clipSize-1) % clipSize), clipEdges(i), ε))
  }
}

object SutherlandHodgemanClipper {
  lazy val singleton = new SutherlandHodgemanClipper
  def clip(subject:Polygon2D, clipPolygon:Polygon2D, ε:Double=Polygon2D.ε)=Polygon2D(singleton.clipPolygon(subject.toIndexedSeq, clipPolygon.toIndexedSeq,ε), ε)
  def clip(subject:IndexedSeq[Vec2D], clipEdges:IndexedSeq[Vec2D], ε:Double)=singleton.clipPolygon(subject, clipEdges, ε)
  def clip(subject:IndexedSeq[Vec2D], clipEdge:FiniteLine2D, ε:Double)=singleton.clipPolygon(subject, clipEdge, ε)
  def clip(subject:IndexedSeq[Vec2D], clipV1:Vec2D, clipV2:Vec2D, ε:Double)=singleton.clipPolygon(subject, clipV1, clipV2, ε)
}