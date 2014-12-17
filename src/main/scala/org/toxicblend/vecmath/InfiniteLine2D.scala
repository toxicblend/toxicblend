package org.toxicblend.vecmath

class InfiniteLine2D(a:Vec2D, b:Vec2D) extends Line2D(a,b) {
  
  def intersectLine(l:InfiniteLine2D) = InfiniteLine2D.intersectLine(a, b, l.a, l.b)
  def intersectLine(v1:Vec2D, v2:Vec2D) = InfiniteLine2D.intersectLine(a, b, v1, v2)
  def sqrDistanceToPoint(p:Vec2D, ε:Double=Polygon2D.ε) = InfiniteLine2D.sqrDistanceToPoint(p, a, b, ε)
   
  override def toString = a.toString + "->" + b.toString
}

object InfiniteLine2D {
  
  def apply(a:Vec2D, b:Vec2D) = new InfiniteLine2D(a,b)
  def apply(x1:Double, y1:Double, x2:Double, y2:Double ) = new InfiniteLine2D(Vec2D(x1,y1), Vec2D(x2,y2))
  
  def sqrDistanceToPoint(p:Vec2D, s1:Vec2D, s2:Vec2D, ε:Double=Polygon2D.ε): Double = {
    if (s1.=~=(s2, Polygon2D.ε)) s1.distanceToSquared(p)
    else {
      val u = ((p.x-s1.x)*(s2.x-s1.x)+(p.y-s1.y)*(s2.y-s1.y))/s1.distanceToSquared(s2)
      val x = s1.x + u*(s2.x-s1.x)
      val y = s1.y + u*(s2.y-s1.y)
      p.distanceToSquared(x,y)
    }
  }
  
  /**
   * from http://paulbourke.net/geometry/pointlineplane/
   */
  def intersectLine(a:Vec2D, b:Vec2D, p:Vec2D, q:Vec2D):Option[Intersection] = {
    val denom = (q.y - p.y) * (b.x - a.x) - (q.x - p.x) * (b.y - a.y)
    val na = (q.x - p.x) * (a.y - p.y) - (q.y - p.y) * (a.x - p.x)
    val nb = (b.x - a.x) * (a.y - p.y) - (b.y - a.y) * (a.x - p.x)

    if (denom != 0.0) {
      val ua = na / denom
      //val ub = nb / denom
      Option(SimpleIntersection(a.interpolateTo(b, ua)))
      //if (math.abs(ua) > math.abs(ub)) Option(SimpleIntersection(a.interpolateTo(b, ua)))
      //else Option(SimpleIntersection(p.interpolateTo(q, ub)))
    } else if (na == 0.0 && nb == 0.0) {
      Option(CoincidentIntersection(a,b))
    } else None
  }
}

object InfiniteLine2DTest extends App {
  
  val l1 = InfiniteLine2D(Vec2D(0.5,0), Vec2D(1,0))
  val l2 = InfiniteLine2D(Vec2D(0,0), Vec2D(0,1))
  val i = l1.intersectLine(l2)
  if (i.isDefined) 
    println(i)
}