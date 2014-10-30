package org.toxicblend.operations.meshgenerator.vecmath

class Polygon2D (val vertices:IndexedSeq[Vec2D], val ε:Double = Polygon2D.ε) {
  val size = vertices.size
   
  lazy val bounds = {
    val min = new MutableVec2D(Double.PositiveInfinity, Double.PositiveInfinity)
    val max = new MutableVec2D(Double.NegativeInfinity, Double.NegativeInfinity)
    vertices.foreach(v => {
      if (v.x > max.x) max.x = v.x
      else if (v.x < min.x) min.x = v.x
      if (v.y > max.y) max.y = v.y
      else if (v.y < min.y) min.y = v.y
    })
    new AABB2D(min,max)
  }
  
  lazy val (minCenterDistanceSquared, centerIsInside) = {
    val center = bounds
    val size = vertices.size
    var prevI = size-1
    var rv = Double.PositiveInfinity
    
    (0 until size).foreach(i=>{
      val d = FiniteLine2D.sqrDistanceToPoint(center, vertices(prevI), vertices(i))
      if (d < rv) rv = d
      prevI = i
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
    var iPrev = size-1
    (0 until size).foreach ( i=> {
        val a = vertices(i)
        val b = vertices(iPrev)
        area += a.x * b.y;
        area -= a.y * b.x;
        iPrev = i
    })
    area*0.5d
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