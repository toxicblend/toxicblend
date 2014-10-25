package org.toxicblend.operations.meshgenerator.vecmath

class Polygon2D (val vertices:IndexedSeq[Vec2D]) {
  val size = vertices.size
  
  /*
   * ported from toxiclibs Polygon2D.
   * There seems to be a numerical problem when the point is on an edge
   */
  def containsPoint(p:Vec2D):Boolean = {
    
    var oddNodes = false
    var vj = vertices.last
    val px = p.x
    val py = p.y
    vertices.foreach(vi => {
      if (sqrDistanceToClosestPoint(p, vj, vi) <= Polygon2D.ε ) {
        //println("containsPoint2 = true" + p + " vj:" + vj + " vi:" + vi)
        return true
      }
      if (vi.y < py && vj.y >= py || vj.y < py && vi.y >= py) {
        if (vi.x + (py - vi.y) / (vj.y - vi.y) * (vj.x - vi.x) < px) {
            oddNodes = !oddNodes
        }
      }
      vj = vi
    })
    //if (p.x == 100d && p.y == 250d) {
    //  println("clip contains " + p + "=" + oddNodes)
    //}
    oddNodes
  }
  
  protected def rayIntersectsSegment(p:Vec2D, s1:Vec2D, s2:Vec2D): Boolean = {
    //A : the end-point of the segment with the smallest y coordinate
    //    (A must be "below" B)
    //B : the end-point of the segment with the greatest y coordinate
    //    (B must be "above" A)
    
    val (a,b) = if (s1.y < s2.y) (s1,s2)
    else if (s2.y < s1.y) (s2,s1)
    else (s1.add(0,Polygon2D.ε),s2)
    
    if (p.y < a.y || p.y > b.y) return false
    else if (p.x > math.max(a.x, b.x)) return false
    else if (p.x < math.min(a.x, b.x)) return true
    else {
      val m_red = if (a.x != b.x) (b.y - a.y)/(b.x - a.x)
                  else Double.PositiveInfinity 
        
      val m_blue = if (a.x != p.x) (p.y - a.y)/(p.x - a.x)
                  else Double.PositiveInfinity
      if (m_blue >= m_red ) true
      else false
    }
  }
  
  protected def sqrDistanceToClosestPoint(p:Vec2D, s1:Vec2D, s2:Vec2D): Double = {
    if (s1.=~=(s2, Polygon2D.ε)) s1.distanceToSquared(p)
    else {
      val u = ((p.x-s1.x)*(s2.x-s1.x)+(p.y-s1.y)*(s2.y-s1.y))/s1.distanceToSquared(s2)
      val x = s1.x + u*(s2.x-s1.x)
      val y = s1.y + u*(s2.y-s1.y)
      p.distanceToSquared(x,y)
    }
  }
  
  def containsPoint2(p:Vec2D) : Boolean = {
    var oddNodes = false
    var vj = vertices.last
    val px = p.x
    val py = p.y
    vertices.foreach(vi => {
      if (p.x == 100d && p.y == 250d) {
        println("ray test. p=" + p + " vj=" + vj + " vi=" + vi + " " + rayIntersectsSegment(p, vj, vi))
      }
      if (sqrDistanceToClosestPoint(p, vj, vi) <= Polygon2D.ε ) {
        println("containsPoint2 = true" + p + " vj:" + vj + " vi:" + vi)
        return true
      }
      if (rayIntersectsSegment(p, vj, vi)) oddNodes = !oddNodes
      vj = vi
    })
    if (p.x == 100d && p.y == 250d) {
      println("point:" + p + " ray=" + rayIntersectsSegment(p, ImmutableVec2D(100.0,300.0),ImmutableVec2D(100.0,100.0) ))
      println("clip contains " + p + "=" + oddNodes + " edges " + vertices.mkString(","))
    }
    oddNodes
  }
}

object Polygon2D {
  val ε = 0.00000001
}