package org.toxicblend.operations.meshgenerator.vecmath

/**
 * A 2 dimensional polygon representation.
 * Any point within ε distance from an edge is considered 'inside' the polygon
 */
class Polygon2D(val vertices:IndexedSeq[Vec2D], val ε:Double = Polygon2D.ε) {
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
        val v = vertices(prev)      // == x(i)  
        val vp1 = vertices(current) // == x(i+1)
        
        area += v.x * vp1.y - vp1.x * v.y
        val m = v.x*vp1.y-vp1.x*v.y
        cx += (v.x + vp1.x) * m
        cy += (v.y + vp1.y) * m
        prev = current
    })
    area = area*0.5d
    //assert(area == getArea, "area:" + area + " != " + "getArea:" + getArea)
    Vec2D(cx/(6d*area), cy/(6d*area))
  }
  
  /**
   * Returns true if the polygon is clockwise
   * Code ported from: http://paulbourke.net/geometry/polygonmesh/
   * (0,0),(1,0),(0,1) is, imho, a clockwise polygon
   */
  def isClockwise = getArea > 0d
  
  def isSelfIntersecting:Boolean = {
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
  
  /**
   * Checks if the polygon is convex.
   * Ported from toxiclibs
   * 
   * @return true, if convex.
   */
  def isConvex:Boolean = {
    var isPositive = false
    val size = vertices.size
    var prev = size-2
    var i = size-1
    (0 until size).foreach(next => {
        //println("prev="+prev+" i="+i+" next="+next)
        val d0 = vertices(i).sub(vertices(prev))
        val d1 = vertices(next).sub(vertices(i))
        val newIsP = d0.cross(d1) > 0
        if (next == 0) isPositive = newIsP
        else if (isPositive != newIsP) return false
        prev = i
        i = next
    })
    return true;
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
  def toConvexHull:Polygon2D = {
    
    var pointOnHull = vertices.foldLeft(vertices.head)((b,a) => if (a.x < b.x) a else b)
    var endPoint = pointOnHull
    val rv = new collection.mutable.ArrayBuffer[Vec2D](1)
    val size = this.vertices.size
       
    do {
      rv.append(pointOnHull)
      (0 until size).foreach(j=>{
          if (endPoint==pointOnHull) endPoint=vertices(j)
          else 
            if (Vec2D.cross(rv.last, endPoint, vertices(j)) < 0d) endPoint = vertices(j)
        })
      pointOnHull = endPoint
    } while (endPoint!=rv(0))
    new Polygon2D(rv)
  }
  
  /**
   * Compute the convex hull using the Andrew's monotone chain convex hull algorithm
   * 
   * http://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
   */
  def toConvexHull2:Polygon2D = {
    
    val sortedV = vertices.sortWith{(a,b) => 
      val cmpX=a.x-b.x
      (if (cmpX==0) a.y-b.y else cmpX) > 0
    }
    val size = sortedV.size
    
    if (size>=3) {
      var k = 0
      val rv = new Array[Vec2D](size*2)
    
      // Build lower hull
      (0 until size).foreach(i => {
        while (k >= 2 && Vec2D.cross(rv(k - 2), rv(k - 1), sortedV(i)) <= 0)
          k -= 1
        
        rv(k) = sortedV(i)
        k+=1
      })
      
      // Build upper hull
      val t = k+1
      for (i<- (size-2) to 0 by -1) {
        while (k >= t && Vec2D.cross(rv(k - 2), rv(k - 1), sortedV(i)) <= 0)
          k-=1
        rv(k) = sortedV(i)
        k+=1
      }
      return new Polygon2D ( if (k > 1) rv.slice(0, k - 1) else rv)
    }
    this
  }
  
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
  
  def containsPoint(p:Vec2D):Boolean = {
    if ( ! bounds.containsPoint(p) ) return false
    if ( minCenterDistanceSquared.isDefined && p.distanceToSquared(bounds) <= minCenterDistanceSquared.get) return centerIsInside
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
  
  def apply(vertices:IndexedSeq[Vec2D], ε:Double = Polygon2D.ε) = {
    
    def containsIdenticalConsecutivePoints:Boolean = {
      vertices.sliding(2).foreach( v=> {
        if (v(0).=~=(v(1),ε)) return true 
      })
      false 
    }
    if (vertices.size >= 2 && containsIdenticalConsecutivePoints) {
      val buffer = new collection.mutable.ArrayBuffer[Vec2D](vertices.size-1)
      buffer.append(vertices.head)
      (1 until vertices.size).foreach(i=> {
        if (! vertices(i).=~=(buffer.last, ε)) {
          buffer.append(vertices(i))
        }
      })
      new Polygon2D(buffer,ε)
    } else {
      new Polygon2D(vertices,ε)
    } 
  }
}