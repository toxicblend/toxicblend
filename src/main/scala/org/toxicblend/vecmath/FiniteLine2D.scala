package org.toxicblend.vecmath

import scala.IndexedSeq

sealed abstract class Intersection
sealed case class SimpleIntersection(val p:Vec2D) extends Intersection
sealed case class CoincidentIntersection(val a:Vec2D, val b:Vec2D) extends Intersection

class FiniteLine2D(val a:Vec2D, val b:Vec2D) {
  
  def getDirection: Vec2D = b.sub(a).normalize
  
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
  def intersectCoincidentLine(a1:Vec2D, a2:Vec2D, b1:Vec2D, b2:Vec2D) = {

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
  
  def intersects(that:FiniteLine2D): Boolean = FiniteLine2D.intersects(a, b, that.a, that.b)
  def sqrDistanceToPoint(p:Vec2D) = FiniteLine2D.sqrDistanceToPoint(p,a,b)
  def distanceToPoint(p:Vec2D) = math.sqrt(FiniteLine2D.sqrDistanceToPoint(p,a,b))
  
  override def toString = a.toString + "->" + b.toString
}

object FiniteLine2D {
  
  def apply(s1:Vec2D, s2:Vec2D) = new FiniteLine2D(s1, s2)
  
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
  
  @inline def intersects(aa:Vec2D, ab:Vec2D, ba:Vec2D, bb:Vec2D): Boolean = {
    if (Vec2D.ccw(aa, ab, ba) * Vec2D.ccw(aa, ab, bb) > 0) return false
    if (Vec2D.ccw(ba, bb, aa) * Vec2D.ccw(ba, bb, ab) > 0) return false
    true
  }
  
}