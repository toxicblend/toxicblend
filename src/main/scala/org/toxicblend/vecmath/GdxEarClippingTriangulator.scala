package org.toxicblend.vecmath

import com.badlogic.gdx.math.{EarClippingTriangulator=>GDXEarClippingTriangulator}

/**
 * until a clipper in scala is available this will have to suffice
 * This class is not thread safe
 */
class GdxEarClippingTriangulator{
  protected val ect = new GDXEarClippingTriangulator
  
  def triangulatePolygon(p:Polygon2D):IndexedSeq[IndexedSeq[Vec2D]] = {
    val vertices = new Array[Float](p.size*2)
    (0 until p.size).foreach(i =>{
      vertices(2*i) = p.vertices(i).x.toFloat
      vertices(2*i+1) = p.vertices(i).y.toFloat
    })
    val result = ect.computeTriangles(vertices,0,p.size*2)
    val numberOfPolygons = result.size/3
    val rv = new Array[IndexedSeq[Vec2D]](numberOfPolygons)
    for (i <- 0 until numberOfPolygons) { 
      rv(i) = IndexedSeq( p.vertices(result.get(i*3)), p.vertices(result.get(i*3 + 1)), p.vertices(result.get(i*3+2)))
    }
    rv
  }
} 

