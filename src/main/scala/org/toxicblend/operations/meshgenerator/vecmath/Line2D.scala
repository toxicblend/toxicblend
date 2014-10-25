package org.toxicblend.operations.meshgenerator.vecmath

class Line2D(val a:Vec2D, val b:Vec2D) {
  
  def intersectLine(l:Line2D):Option[Vec2D] = {
    val denom = (l.b.y - l.a.y) * (b.x - a.x) - (l.b.x - l.a.x) * (b.y - a.y)
    val na = (l.b.x - l.a.x) * (a.y - l.a.y) - (l.b.y - l.a.y) * (a.x - l.a.x)
    val nb = (b.x - a.x) * (a.y - l.a.y) - (b.y - a.y) * (a.x - l.a.x)

    if (denom != 0.0) {
      val ua = na / denom
      val ub = nb / denom
      if (ua >= 0.0f && ua <= 1.0 && ub >= 0.0 && ub <= 1.0) {
          Option(a.interpolateTo(b, ua))
      } else None
    } else if (na == 0.0 && nb == 0.0) {
      Option(a)
    } else None
  }
  
  override def toString = a.toString + "->" + b.toString
}