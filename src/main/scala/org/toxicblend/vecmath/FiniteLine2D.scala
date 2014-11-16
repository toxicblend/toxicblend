package org.toxicblend.vecmath

import scala.IndexedSeq

sealed abstract class Intersection
sealed case class SimpleIntersection(val p:Vec2D) extends Intersection
sealed case class CoincidentIntersection(val a:Vec2D, val b:Vec2D) extends Intersection

class FiniteLine2D(val a:Vec2D, val b:Vec2D) {
  
  def getDirection: Vec2D = b.sub(a).normalized
  
  protected def overlapIntervals(ub1:Double, ub2:Double) = {
    val l = Math.min(ub1, ub2)
    val r = Math.max(ub1, ub2)
    val a = Math.max(0, l)
    val b = Math.min(1, r)
    if (a > b) // no intersection
      IndexedSeq[Double]()
    else if (a == b)
      IndexedSeq( a )
    else // if (a < B)
      IndexedSeq( a, b)
  }

  // IMPORTANT: a1 and a2 cannot be the same, e.g. a1--a2 is a true segment, not a point
  // b1/b2 may be the same (b1--b2 is a point)
  protected def intersectCoincidentLine(a1:Vec2D, a2:Vec2D, b1:Vec2D, b2:Vec2D) = {

    assert(a1!=a2)
    val denomx = a2.x - a1.x
    val denomy = a2.y - a1.y

    val (ub1, ub2) = if (Math.abs(denomx) > Math.abs(denomy)) {
      ( (b1.x - a1.x) / denomx, (b2.x - a1.x) / denomx)
    } else {
      ( (b1.y - a1.y) / denomy, (b2.y - a1.y) / denomy)
    }

    overlapIntervals(ub1, ub2).map( f=> {
      val x = a2.x * f + a1.x * (1d - f)
      val y = a2.y * f + a1.y * (1d - f)
      Vec2D(x, y)
    })
  }
   
  def intersectLine(l:FiniteLine2D): Option[Intersection] = {
    val denom = (l.b.y - l.a.y) * (b.x - a.x) - (l.b.x - l.a.x) * (b.y - a.y)
    val na = (l.b.x - l.a.x) * (a.y - l.a.y) - (l.b.y - l.a.y) * (a.x - l.a.x)
    val nb = (b.x - a.x) * (a.y - l.a.y) - (b.y - a.y) * (a.x - l.a.x)

    if (denom != 0.0) {
      val ua = na / denom
      val ub = nb / denom
      if (ua >= 0d && ua <= 1d && ub >= 0d && ub <= 1d) {
        val intersection = a.interpolateTo(b, ua)
        //println("" + this + " intersects " + l + " at " + intersection)
        Option(new SimpleIntersection(intersection))
      } else return None
    } else if (na == 0.0 && nb == 0.0) {
      val r = intersectCoincidentLine(a, b, l.a, l.b)
      //if (r.size>0) println("" + this + " is coincident with " + l )
      //else println("" + this + " is coincident with " + l + " but no intersection")
      
      r.size match {
        case 0 => None
        case 1 => Option(new SimpleIntersection(r(0)))
        case 2 => Option(new CoincidentIntersection(r(0), r(1)))
        case _ => assert(false, "should never happen"); None
      }
    } else None
  }
  
  def middlePoint = FiniteLine2D.middlePoint(a,b) 
  def intersects(that:FiniteLine2D): Boolean = FiniteLine2D.intersects(a, b, that.a, that.b)
  def sqrDistanceToPoint(p:Vec2D) = FiniteLine2D.sqrDistanceToPoint(p,a,b)
  def distanceToPoint(p:Vec2D) = math.sqrt(FiniteLine2D.sqrDistanceToPoint(p,a,b))
  def add(v:Vec2D) = FiniteLine2D(a.add(v), b.add(v))
  def sub(v:Vec2D) = FiniteLine2D(a.sub(v), b.sub(v))
  override def toString = a.toString + "->" + b.toString
}

object FiniteLine2D {
  
  def apply(s1:Vec2D, s2:Vec2D) = new FiniteLine2D(s1, s2)
  
  def apply(x1:Double, y1:Double,x2:Double, y2:Double ) = new FiniteLine2D(Vec2D(x1,y1), Vec2D(x2,y2))
  
  @inline def sqrDistanceToPoint(p:Vec2D, s1:Vec2D, s2:Vec2D, ε:Double=Polygon2D.ε): Double = {
    if (s1.=~=(s2, Polygon2D.ε)) s1.distanceToSquared(p)
    else {
      val u = ((p.x-s1.x)*(s2.x-s1.x)+(p.y-s1.y)*(s2.y-s1.y))/s1.distanceToSquared(s2)
      if (u <= 0) {
        p.distanceToSquared(s1)
      } else if (u >= 1d) {
        p.distanceToSquared(s2)
      } else {
        val x = s1.x + u*(s2.x-s1.x)
        val y = s1.y + u*(s2.y-s1.y)
        p.distanceToSquared(x,y)
      }
    }
  }
  
  
  
  /**
   * From http://stackoverflow.com/questions/7069420/check-if-two-line-segments-are-colliding-only-check-if-they-are-intersecting-n
   * 
   * To see whether two points, P and Q, are on different sides of a line segment [EF], compute two cross products, one for P and one for Q:
   *   (Fx - Ex)(Py - Fy) - (Fy - Ey)(Px - Fx)
   *   (Fx - Ex)(Qy - Fy) - (Fy - Ey)(Qx - Fx)
   * If the results have the same sign (both positive or both negative) then forget it, 
   * the points are on the same side, If one is positive and the other negative, then the points are on opposite sides
   */
  @inline def differentSide(p:Vec2D, q:Vec2D, e:Vec2D, f:Vec2D): Boolean = {
    ((f.x - e.x)*(p.y - f.y) - (f.y - e.y)*(p.x - f.x)) * ((f.x - e.x)*(q.y - f.y) - (f.y - e.y)*(q.x - f.x)) < 0d
  }
  
  /**
   * From http://stackoverflow.com/questions/7069420/check-if-two-line-segments-are-colliding-only-check-if-they-are-intersecting-n
   * 
   * suppose you're looking at two line segments, [AB] and [CD]. 
   * The segments intersect if and only if ((A and B are of different sides of [CD]) and (C and D are on different sides of [AB])).
   * the segments do not intersect. 
   */
  @inline def intersects(aa:Vec2D, ab:Vec2D, ba:Vec2D, bb:Vec2D): Boolean = differentSide(aa,ab,ba,bb) && differentSide(ba,bb,aa,ab)
  
  def intersectLine(aa:Vec2D, ab:Vec2D, ba:Vec2D, bb:Vec2D): Option[Intersection] = FiniteLine2D(aa,ab).intersectLine(FiniteLine2D(ba,bb))
  
  /**
   * @return true if the vectors a->b and b->c are collinear (ignoring direction)
   */
  @inline def areCollinear(a:Vec2D, b:Vec2D, c:Vec2D, ε:Double=Polygon2D.ε): Boolean = {
    if (a.=~=(b,ε) || b.=~=(c,ε)) return true
    val d1x = a.x-b.x
    val d1y = a.y-b.y
    val d2x = b.x-c.x
    val d2y = b.y-c.y
    val dot = Vec2D.dot(d1x,d1y,d2x,d2y)/(Vec2D.magnitude(d1x, d1y)*Vec2D.magnitude(d2x, d2y))
    if (dot > 0.5) return (dot-1d).abs < ε
    else if (dot < -0.5) return (dot+1d).abs < ε
    else false
  }
  
  /**
   * @return true if the vectors a->b and b->c are collinear and point in the same direction 
   */
  @inline def areCollinearSameDirection(a:Vec2D, b:Vec2D, c:Vec2D, ε:Double=Polygon2D.ε): Boolean = {
    if (a.=~=(b,ε) || b.=~=(c,ε)) return true
    val d1x = a.x-b.x
    val d1y = a.y-b.y
    val d2x = b.x-c.x
    val d2y = b.y-c.y
    val dotm1 = Vec2D.dot(d1x,d1y,d2x,d2y)/(Vec2D.magnitude(d1x, d1y)*Vec2D.magnitude(d2x, d2y)) -1
    dotm1 <= ε && dotm1 >= -ε
  }
  
  /**
   * @return the point in the middle of a and b
   */
  @inline def middlePoint(a:Vec2D, b:Vec2D):Vec2D = Vec2D(a.x+0.5*(b.x-a.x),a.y+0.5*(b.y-a.y))
}
