package org.toxicblend.vecmath

abstract class Line2D(val a:Vec2D, val b:Vec2D) {
  def getDirection: Vec2D = b.sub(a).normalized
  def intersectLine(v1:Vec2D, v2:Vec2D):Option[Intersection]
  def sqrDistanceToPoint(p:Vec2D, ε:Double=Polygon2D.ε):Double
}
