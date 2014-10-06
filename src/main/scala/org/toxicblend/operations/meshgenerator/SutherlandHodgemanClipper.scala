package org.toxicblend.operations.meshgenerator

import toxi.geom.Vec3D
import toxi.geom.Vec2D
import toxi.geom.Line2D
import toxi.geom.Line2D.LineIntersection.Type.NON_INTERSECTING
import toxi.geom.Line2D.LineIntersection.Type.INTERSECTING
import toxi.geom.ReadonlyVec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Polygon2D

import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConversions._


/**
 *     
 *     public Polygon2D clipPolygon(Polygon2D poly) {
        List<Vec2D> points = new ArrayList<Vec2D>(poly.vertices);
        List<Vec2D> clipped = new ArrayList<Vec2D>();
        points.add(points.get(0));
        for (int edgeID = 0; edgeID < 4; edgeID++) {
            clipped.clear();
            for (int i = 0, num = points.size() - 1; i < num; i++) {
                Vec2D p = points.get(i);
                Vec2D q = points.get(i + 1);
                if (isInsideEdge(p, edgeID)) {
                    if (isInsideEdge(q, edgeID)) {
                        clipped.add(q.copy());
                    } else {
                        clipped.add(getClippedPosOnEdge(edgeID, p, q));
                    }
                    continue;
                }
                if (isInsideEdge(q, edgeID)) {
                    clipped.add(getClippedPosOnEdge(edgeID, p, q));
                    clipped.add(q.copy());
                }
            }
            if (clipped.size() > 0
                    && clipped.get(0) != clipped.get(clipped.size() - 1)) {
                clipped.add(clipped.get(0));
            }
            List<Vec2D> t = points;
            points = clipped;
            clipped = t;
        }
        return new Polygon2D(points).removeDuplicates(0.001f);
    }
 * 
 */
class SutherlandHodgemanClipper( ) {
  val center:ReadonlyVec2D = new Vec2D
  
  def clockwiseFactor(edge:Line2D) = {
    /* val realSign = {
      val poly = new Polygon2D
      poly.add(edge.a)
      poly.add(edge.b)
      poly.add(center.copy)
      if (poly.isClockwise) -1f else 1f
    } */
    val sign = if (edge.b.sub(edge.a).cross(center.sub(edge.a)) >= 0 ) -1f else 1f
    //if (sign != realSign) println("sign != realSign")
    -sign
  }
  
  def isInside(point:ReadonlyVec2D, edge:Line2D, sign:Float):Boolean = {
    val cross = edge.b.sub(edge.a).cross(point.sub(edge.a))
    val rv = sign*cross >= 0
    /*val anotherRv = {
      val cp = getClippedPosOnEdge(edge, point, center)
      cp.isDefined && cp.get.sub(center).magSquared <= point.sub(center).magSquared
    }
    if (rv != anotherRv)
      println("isInside fail: point=" + point + " edge=" + edge + " center=" + center + " rv=" + rv + " anotherRv=" + anotherRv)
    //else 
    //  println("isInside ok: point=" + point + " edge=" + edge + " center=" + center + " rv=" + rv + " anotherRv=" + anotherRv)
    anotherRv*/
    rv
  }
  
  def getClippedPosOnEdge(edge:Line2D, p1:ReadonlyVec2D, p2:ReadonlyVec2D): Option[ReadonlyVec2D] = {
    val inter = edge.intersectLine(new Line2D(p1,p2))
    val iType = inter.getType
    if (iType == INTERSECTING ) {
      Option(inter.getPos)
    } else if (iType == NON_INTERSECTING ) {
      val ub = inter.getCoefficients()(1)
      if (ub < 0f || ub > 1f) {
        Option(inter.getPos)
      } else None
    } else None
  }
  
  /**
   * Returns the intersection point between an infinite line and one finite line (segment)
   */
  def getIntersectionPosOnInfiniteLine(infiniteLine:Line2D, fromV:ReadonlyVec2D, toV:ReadonlyVec2D): Option[ReadonlyVec2D] = {
    val x1 = infiniteLine.a.x.toDouble
    val y1 = infiniteLine.a.y.toDouble
    val x2 = infiniteLine.b.x.toDouble
    val y2 = infiniteLine.b.y.toDouble
    val x3 = fromV.x.toDouble
    val y3 = fromV.y.toDouble
    val x4 = toV.x.toDouble
    val y4 = toV.y.toDouble
    
    val denom = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)
    val nb = (x2-x1)*(y1-y3)-(y2-y1)*(x1-x3) // edge line position
    
    if (denom != 0.0) {
      //val ua = na / denom
      val ub = nb / denom
      if (ub >= 0 && ub <= 1) {
        val i = fromV.interpolateTo(toV, ub.toFloat);
        Some(i)  
      } else {
        None 
      }
    } else {
      val na = (x4-x3)*(y1-y3)-(y4-y3)*(x1-x3) // infinite line position
      if (na == 0.0 && nb == 0.0) {
        if (infiniteLine.distanceToPoint(fromV) == 0.0) {
          Some(fromV) // COINCIDENT. Any point on edge is intersecting - what to return?
        } else {
          None // COINCIDENT_NO_INTERSECT
        }
      } else {
        None // PARALLEL
      }
    }
  }
  
  /**
   * Clips a polygon with regards to one single edge.
   * Origo is assumed to be on the 'inside' side of the edge 
   */
  def clipPolygon(poly:Polygon2D, edge:Line2D ) : Polygon2D = {
    val points = new ArrayBuffer[ReadonlyVec2D](poly.size+1); points.insertAll(0,poly.vertices.toTraversable)
    val output = new ArrayBuffer[ReadonlyVec2D] // outputList
    val sign = clockwiseFactor(edge)
      
    (0 until points.size).foreach( i => {
      //val e = p(1)
      //val s = p(0)
      val current = points(i)
      val previous = if (i==0) points.last else points(i-1)
      
      val currentInside= isInside(current, edge, sign)
      val previousInside = isInside(previous, edge, sign)
      //println("current=\t" + current + "\tinside:" + currentInside)
      //println("   prev=\t" + previous + "\tinside:" + previousInside)
      
      if (currentInside) {
         if (!previousInside) {
           output.add(getIntersectionPosOnInfiniteLine(edge, previous, current).get) 
           //println("\tadd clip")
         }
         output.add(current)
         //println("\tadd current")
      } else if (previousInside) {
        output.add(getIntersectionPosOnInfiniteLine(edge, previous, current).get)
        //println("\tadd clip")
      } else {
        //println("\tnop")
      }
      //if (output.size > 0 && output.head != output.last) {
      //  output += output.head
      //}
    })
    
    new Polygon2D(output.iterator).removeDuplicates(0.001f);
  }
}

object Test extends App {
  
  val p0 = new Vec2D(1,-1)
  val p1 = new Vec2D(-1,-1)
  val p2 = new Vec2D(-1,1)
  val p3 = new Vec2D(1,1)
  
  var polygon = new Polygon2D
  Array(p0.scale(10), p1.scale(10), p2.scale(10), p3.scale(10)).foreach( v => polygon.add(v))
  val clipper = new SutherlandHodgemanClipper
  
  println(polygon)
  println
  
  var iLine = new Line2D(p0, p1)
  polygon = clipper.clipPolygon(polygon, iLine)
  println("line=" + iLine)
  println(polygon)
  println
  
  iLine = new Line2D(p1, p2)
  polygon = clipper.clipPolygon(polygon, iLine)
  println("line=" + iLine)
  println(polygon)
  println
  
  iLine = new Line2D(p2, p3)
  val insideSign = clipper.clockwiseFactor(iLine)
  println("p0: " + clipper.isInside(p0, iLine, insideSign))
  println("p1: " + clipper.isInside(p1, iLine, insideSign))
  println("p2: " + clipper.isInside(p2, iLine, insideSign))
  println("p3: " + clipper.isInside(p3, iLine, insideSign))
  polygon = clipper.clipPolygon(polygon, iLine)
  println("line=" + iLine)
  println(polygon)
  println
  
  iLine = new Line2D(p3, p0)
  polygon = clipper.clipPolygon(polygon, iLine)
  println("line=" + iLine)
  println(polygon)
  println
  println("end")
}