package org.toxicblend.operations.meshgenerator.vecmath

class InfiniteLine2D(val a:Vec2D, val b:Vec2D) {
  
  def intersectLine(l:FiniteLine2D): Option[Vec2D] = {
    val denom = (l.b.y - l.a.y) * (b.x - a.x) - (l.b.x - l.a.x) * (b.y - a.y)
    val na = (l.b.x - l.a.x) * (a.y - l.a.y) - (l.b.y - l.a.y) * (a.x - l.a.x)
    val nb = (b.x - a.x) * (a.y - l.a.y) - (b.y - a.y) * (a.x - l.a.x)

    if (denom != 0.0) {
      val ua = na / denom
      val ub = nb / denom
      if (ua >= ub) Option(a.interpolateTo(b, ua))
      else Option(b.interpolateTo(a, ub))
    } else if (na == 0.0 && nb == 0.0) {
      Option(a)
    } else None
  }
  
  override def toString = a.toString + "->" + b.toString
}

object InfiniteLine2D {
  
  @inline def sqrDistanceToPoint(p:Vec2D, s1:Vec2D, s2:Vec2D, ε:Double=Polygon2D.ε): Double = {
    if (s1.=~=(s2, Polygon2D.ε)) s1.distanceToSquared(p)
    else {
      val u = ((p.x-s1.x)*(s2.x-s1.x)+(p.y-s1.y)*(s2.y-s1.y))/s1.distanceToSquared(s2)
      val x = s1.x + u*(s2.x-s1.x)
      val y = s1.y + u*(s2.y-s1.y)
      p.distanceToSquared(x,y)
    }
  }
}