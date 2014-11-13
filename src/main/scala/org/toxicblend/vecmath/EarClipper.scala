package org.toxicblend.vecmath

import com.badlogic.gdx.math.EarClippingTriangulator

class EarClipper{
  protected val ect = new EarClippingTriangulator
  
  def triangulatePolygon(p:Polygon2D):IndexedSeq[IndexedSeq[Vec2D]] = {
    val vertices = new Array[Float](p.size*2)
    (0 until p.size).foreach(i =>{
      vertices(2*i) = p.vertices(i).x.toFloat
      vertices(2*i+1) = p.vertices(i).y.toFloat
    })
    val result = ect.computeTriangles(vertices,0,p.size*2)
    val rv = new collection.mutable.ArrayBuffer[IndexedSeq[Vec2D]](result.size*3)
    (0 until result.size/3).foreach (i => 
      rv.append( IndexedSeq( p.vertices(result.get(i*3)), p.vertices(result.get(i*3 + 1)), p.vertices(result.get(i*3+2))))
    )
    rv
  }
} 

object EarClipTest extends App {
  def toPolygon2D(seq:Seq[(Double,Double)], scale:Double=1d, x:Double=0d, y:Double=0d):Polygon2D = {
    Polygon2D(seq.toIndexedSeq.map(p=>Vec2D(p._1*scale+x,p._2*scale+y)))
  }
  val ect = new EarClipper

  val p = toPolygon2D(Seq((0,0),(2,0),(2,2),(1,1),(0,2)))
  val rv = ect.triangulatePolygon(p)
  println("result:" + rv.mkString("\n"))
}
