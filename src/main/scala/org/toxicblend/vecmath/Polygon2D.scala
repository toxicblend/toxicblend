package org.toxicblend.vecmath

import org.toxicblend.vecmath.Vec2D

//import scala.collection.immutable.IndexedSeq

/**
 * A 2 dimensional polygon representation.
 * Any point within ε distance from an edge is considered 'inside' the polygon
 */
class Polygon2D protected (val vertices:IndexedSeq[Vec2D], val ε:Double = Polygon2D.ε) extends Iterable[Vec2D]{
  override val size = vertices.size
   
  lazy val bounds = AABB2D(vertices)
  
  lazy val (minCenterDistanceSquared, centerIsInside) = {
    val center = bounds
    val size = vertices.size
    var prev = size-1
    var rv = Double.PositiveInfinity
    
    (0 until size).foreach(current=>{
      val distance = FiniteLine2D.sqrDistanceToPoint(center, vertices(prev), vertices(current))
      if (distance < rv) rv = distance
      prev = current
    })
    (rv, realContainsPoint(center) )
  }
  
  def iterator = vertices.iterator
  
  /**
   * Computes the area of the polygon, provided it isn't self intersecting.
   * Code ported from:
   * http://paulbourke.net/geometry/polygonmesh/
   * 
   * @return polygon area
   */
  def getArea:Double = Polygon2D.getArea(vertices)
  
  /**
   * Computes the centroid of the polygon
   * Code ported from: http://paulbourke.net/geometry/polygonmesh/
   * 
   * @return polygon centroid
   */
  def getCentroid = Polygon2D.getCentroid(vertices) 
  
  /**
   * Returns true if the polygon is clockwise
   * Code ported from: http://paulbourke.net/geometry/polygonmesh/
   * (0,0),(0,1),(1,0) is, imho, a clockwise 2D polygon
   */
  def isClockwise = Polygon2D.isClockwise(vertices)
  
  def isSelfIntersecting = Polygon2D.isSelfIntersecting(vertices)
  
  /**
   * Checks if the polygon is convex.
   * Ported from toxiclibs
   * 
   * @return true, if convex.
   */
  def isConvex:Boolean = Polygon2D.isConvex(vertices)
  
  
  def toConvexHull(forceClockwise:Option[Boolean]=None):Polygon2D = 
    Polygon2D(Polygon2D.toConvexHullGiftWrapping(vertices, forceClockwise))
    
  def toConvexHull2(forceClockwise:Option[Boolean]=None):Polygon2D = 
    Polygon2D(Polygon2D.toConvexHullMonotoneChain(vertices, forceClockwise))
  
  /**
   * return the shortest distance to any edge of this polygon to the point
   */
  def sqrDistanceToPoint(p:Vec2D,ε:Double=Polygon2D.ε):Double = {
    val size = vertices.size
    var iPrev = size-1
    var minDistance = Double.PositiveInfinity
    (0 until size).foreach( i => {
      val distance = FiniteLine2D.sqrDistanceToPoint(p, vertices(iPrev), vertices(i), ε)
      if (distance < minDistance) minDistance = distance
    })
    minDistance
  }
      
  /**
   * returns true if any of the edges of the other polygon intersects any edge of this polygon
   * return false if 'this' or 'other' is completely contained inside the other polygon
   */
  def intersects(other:Polygon2D):Boolean = {
    if (!bounds.intersects(other.bounds)) return false
    val sizeThis = vertices.size
    val sizeOther = other.vertices.size
    val tV = this.vertices
    val oV = other.vertices
    var iPrev = sizeThis-1
    var jPrev = sizeOther-1
    (0 until sizeThis).foreach( i => {
      val iV1 = tV(iPrev)
      val iV2 = tV(i)
      (0 until sizeOther).foreach( j => {
        val jV1 = oV(jPrev)
        val jV2 = oV(j)
        if (FiniteLine2D.intersects(iV1, iV2, jV1, jV2)) return true
        jPrev = j
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
  
  /**
   * returns a new polygon with reversed vertex order
   */
  def reverse:Polygon2D = new Polygon2D(vertices.reverse) 
  
  def containsPoint(p:Vec2D):Boolean = {
    if ( ! bounds.containsPoint(p) ) return false
    if ( p.distanceToSquared(bounds) <= minCenterDistanceSquared) return centerIsInside
    else realContainsPoint(p)
  }
  
  /**
   * Returns a new Polygon2D with the vertex array shifted by one, just for testing purposes
   */
  def shift1:Polygon2D = {
    val s = new collection.mutable.ArrayBuffer[Vec2D](vertices.size) 
    s ++= vertices.drop(1)
    s.append(vertices.head)
    new Polygon2D(s)
  }
  
}

object Polygon2D {
  val ε = 0.00000001
  
  def getArea(vertices:IndexedSeq[Vec2D]):Double = {
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
  
  def isClockwise(vertices:IndexedSeq[Vec2D]) = getArea(vertices) < 0d
  
  def isSelfIntersecting(vertices:IndexedSeq[Vec2D]):Boolean = {
    val size = vertices.size
    if (size < 4) return false
    var iPrev = size-1
    var jPrev = 0
    (0 until size).foreach( i => {
      (i+2 until size).foreach(j => {
        
        jPrev = (j + size -1) % size
        if (j!=i && j!=iPrev && jPrev!=i && jPrev!=iPrev)
          if (FiniteLine2D.intersects(vertices(iPrev), vertices(i), vertices(jPrev), vertices(j))) {
            //println("" + iPrev + "->" + i + " intersects " + jPrev + "->" +j )
            //println("" + FiniteLine2D(vertices(iPrev), vertices(i)) + " intersects " + FiniteLine2D(vertices(jPrev), vertices(j)))
            return true
          } //else println("" + iPrev + "->" + i + " does not intersect " + jPrev + "->" +j )
      })
      iPrev = i
    })
    false
  }
  
  def getCentroid(vertices:IndexedSeq[Vec2D]):Vec2D = {
    val size = vertices.size
    var area = 0d
    var cx = 0d
    var cy = 0d
    
    var prev = size-1
    (0 until size).foreach ( current=> {
        val v = vertices(prev)      // == x(i)  
        val vp1 = vertices(current) // == x(i+1)
        
        area += v.x * vp1.y - vp1.x * v.y
        val m = v.x*vp1.y-vp1.x*v.y
        cx += (v.x + vp1.x) * m
        cy += (v.y + vp1.y) * m
        prev = current
    })
    area = area*0.5d
    Vec2D(cx/(6d*area), cy/(6d*area))
  }
  
  def isConvex(vertices:IndexedSeq[Vec2D]):Boolean = {
    var isPositive = false
    val size = vertices.size
    var prev = size-2
    var i = size-1
    (0 until size).foreach(next => {
        val d0 = vertices(i).sub(vertices(prev))
        val d1 = vertices(next).sub(vertices(i))
        val newIsP = d0.cross(d1) > 0
        if (next == 0) isPositive = newIsP
        else if (isPositive != newIsP) return false
        prev = i
        i = next
    })
    return true
  }
  
  private def dropConsecutiveDoubles(vertices:IndexedSeq[Vec2D], ε:Double):IndexedSeq[Vec2D] = {
    
    def containsIdenticalConsecutivePoints:Boolean = {
      val size = vertices.size
      var prev = size-1
      (0 until size).foreach( next => {
        if (vertices(next).=~=(vertices(prev),ε)) return true 
        prev = next
      })
      false 
    }
    
    if (vertices.size >= 2 && containsIdenticalConsecutivePoints) {
      val buffer = new collection.mutable.ArrayBuffer[Vec2D](vertices.size-1)
      buffer.append(vertices.head)
      (1 until vertices.size).foreach(i=> {
        if (! vertices(i).=~=(buffer.last, ε)) {
          buffer.append(vertices(i))
        } else {
          //println("Polygon2D.dropConsecutiveDoubles: removed one superfluous vertex:" + vertices(i))
          //println("Polygon2D.dropConsecutiveDoubles: input was: " + vertices)
        }
      })
      if (buffer.head.=~=(buffer.last, ε)) {
        //println("Polygon2D.dropConsecutiveDoubles removed one superfluous vertex from the end:" + buffer.last)
        buffer.remove(buffer.size-1)
        //println("Polygon2D.dropConsecutiveDoubles: result: " + buffer)
      }
      return buffer
    } else {
      vertices
    } 
  }
  
  /**
   * compute the convex hull using the gift wrapping algorithm
   * http://en.wikipedia.org/wiki/Gift_wrapping_algorithm
   * 
   * pointOnHull = leftmost point in S
   * i = 0
   * repeat
   *    P[i] = pointOnHull
   *    endpoint = S[0]         // initial endpoint for a candidate edge on the hull
   *    for j from 1 to |S|
   *       if (endpoint == pointOnHull) or (S[j] is on left of line from P[i] to endpoint)
   *          endpoint = S[j]   // found greater left turn, update endpoint
   *    i = i+1
   *    pointOnHull = endpoint
   *  until endpoint == P[0]      // wrapped around to first hull point
   */
  def toConvexHullGiftWrapping(vertices:IndexedSeq[Vec2D], forceClockwise:Option[Boolean]=None):IndexedSeq[Vec2D] = {
    
    var pointOnHull = vertices.foldLeft(vertices.head)((b,a) => if (a.x < b.x) a else b)
    var endPoint = pointOnHull
    val rv = new collection.mutable.ArrayBuffer[Vec2D](1)
    val size = vertices.size
       
    do {
      rv.append(pointOnHull)
      (0 until size).foreach(j=>{
          if (endPoint==pointOnHull) endPoint=vertices(j)
          else 
            if (Vec2D.cross(rv.last, endPoint, vertices(j)) < 0d) endPoint = vertices(j)
        })
      pointOnHull = endPoint
    } while (endPoint!=rv(0))
      
    if (forceClockwise.isDefined && forceClockwise.get!=Polygon2D.isClockwise(rv)) return rv.reverse
    rv
  }
  
  /**
   * Compute the convex hull using the Andrew's monotone chain convex hull algorithm
   * 
   * http://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
   * 
   * Code is ported from that java example
   */
  def toConvexHullMonotoneChain(vertices:IndexedSeq[Vec2D], forceClockwise:Option[Boolean]=None):IndexedSeq[Vec2D] = {
    
    val sortedVertices = vertices.sortWith{(a,b) => 
      val cmpX=a.x-b.x
      (if (cmpX==0) a.y-b.y else cmpX) > 0
    }
    val size = sortedVertices.size
    
    if (size>3) {
      var k = 0
      val a = new Array[Vec2D](size*2)
    
      // Build lower hull
      (0 until size).foreach(i => {
        while (k >= 2 && Vec2D.cross(a(k - 2), a(k - 1), sortedVertices(i)) <= 0)
          k -= 1
        
        a(k) = sortedVertices(i)
        k+=1
      })
      
      // Build upper hull
      val t = k+1
      for (i<- (size-2) to 0 by -1) {
        while (k >= t && Vec2D.cross(a(k - 2), a(k - 1), sortedVertices(i)) <= 0)
          k-=1
        a(k) = sortedVertices(i)
        k+=1
      }
      val rv = if (k > 1) a.slice(0, k - 1) else a
      if (forceClockwise.isDefined && forceClockwise.get!=Polygon2D.isClockwise(rv)) return rv.reverse
      rv
    } else 
      return vertices
  }
  
  def apply(vertices:IndexedSeq[Vec2D], ε:Double = Polygon2D.ε) = new Polygon2D(dropConsecutiveDoubles(vertices,ε))

}