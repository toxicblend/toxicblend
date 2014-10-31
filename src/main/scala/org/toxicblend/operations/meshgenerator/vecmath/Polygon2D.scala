package org.toxicblend.operations.meshgenerator.vecmath

class Polygon2D (val vertices:IndexedSeq[Vec2D], val ε:Double = Polygon2D.ε) {
  val size = vertices.size
   
  lazy val bounds = AABB2D(vertices)
  
  lazy val (minCenterDistanceSquared, centerIsInside) = {
    val center = bounds
    val size = vertices.size
    var prev = size-1
    var rv = Double.PositiveInfinity
    
    (0 until size).foreach(current=>{
      val d = FiniteLine2D.sqrDistanceToPoint(center, vertices(prev), vertices(current))
      if (d < rv) rv = d
      prev = current
    })
    (Option(rv), realContainsPoint(center) )
  }
  
  def toIndexedSeq = vertices
  
  /**
   * Computes the area of the polygon, provided it isn't self intersecting.
   * Code ported from:
   * http://paulbourke.net/geometry/polygonmesh/
   * and toxiclibs
   * 
   * @return polygon area
   */
  def getArea:Double = {
    val size = vertices.size
    var area = 0d
    var prev = size-1
    (0 until size).foreach( current=> {
        val v = vertices(prev)      // == v(i)
        val vp1 = vertices(current) // == v(i+1)
        area += v.x * vp1.y - vp1.x * v.y 
        prev = current
    })
    area*0.5d
  }
  
  /**
   * Computes the centroid of the polygon
   * Code ported from: http://paulbourke.net/geometry/polygonmesh/
   * 
   * @return polygon centroid
   */
  def getCentroid:Vec2D = {
    val size = vertices.size
    var area = 0d
    var cx = 0d
    var cy = 0d
    
    var prev = size-1
    (0 until size).foreach ( current=> {
        val v = vertices(prev)    // == x(i)  
        val vp1 = vertices(current) // == x(i+1)
        
        area += v.x * vp1.y - vp1.x * v.y
        val m = v.x*vp1.y-vp1.x*v.y
        cx += (v.x + vp1.x) * m
        cy += (v.y + vp1.y) * m
        prev = current
    })
    area = area*0.5d
    assert(area == getArea, "area:" + area + " != " + "getArea:" + getArea)
    Vec2D(cx/(6d*area), cy/(6d*area))
  }
  
  def isClockwise = getArea > 0d
  
  def isSelfIntersecting:Boolean = {
    val size = vertices.size
    if (size < 4) return false
    var iPrev = size-1
    var jPrev = 0
    (0 until size-1).foreach( i => {
      (i+2 until size-1).foreach(j => {
        jPrev = (j + size -1) % size
        if (FiniteLine2D.intersects(vertices(iPrev), vertices(i), vertices(jPrev), vertices(j))) {
          //println("" + iPrev + "->" + i + " intersects " + jPrev + "->" +j )
          //println("" + FiniteLine2D(vertices(iPrev), vertices(i)) + " intersects " + FiniteLine2D(vertices(jPrev), vertices(j)))
          return true
        } //else println("" + iPrev + "->" + i + " ! intersects " + jPrev + "->" +j )
      })
      iPrev = i
    })
    false
  }
  
  /*
   * ported from toxiclibs Polygon2D.
   */
  protected def realContainsPoint(p:Vec2D):Boolean = {
    
    var oddNodes = false
    var vj = vertices.last
    val px = p.x
    val py = p.y
    vertices.foreach(vi => {
 
      val sqrDistanceToClosestPointSample = FiniteLine2D.sqrDistanceToPoint(p, vj, vi)
      if ( sqrDistanceToClosestPointSample <= ε && math.sqrt(sqrDistanceToClosestPointSample) <= ε ) {
        // point is less than ε length units from the edge 
        // println("distance=" + sqrDistanceToClosestPointSample)
        return true
      }
      if (vi.y < py && vj.y >= py || vj.y < py && vi.y >= py) {
        if (vi.x + (py - vi.y) / (vj.y - vi.y) * (vj.x - vi.x) < px) {
            oddNodes = !oddNodes
        }
      }
      vj = vi
    })
    oddNodes
  }
  
  def containsPoint(p:Vec2D):Boolean = {
    if ( ! bounds.containsPoint(p) ) return false
    if ( minCenterDistanceSquared.isDefined && p.distanceToSquared(bounds) <= minCenterDistanceSquared.get) return centerIsInside
    else realContainsPoint(p)
  }
 
}

object Polygon2D {
  val ε = 0.00000001
}