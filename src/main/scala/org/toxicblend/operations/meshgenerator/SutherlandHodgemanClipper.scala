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
class SutherlandHodgemanClipper {
  val center:ReadonlyVec2D = new Vec2D
    
  def isInside(point:ReadonlyVec2D, edge:Line2D):Boolean = 
    (edge.a.x-point.x)*(edge.b.y-point.y)>(edge.a.y-point.y)*(edge.b.x-point.x)
  
  @inline protected def append(v:Vec2D, buf:ArrayBuffer[Vec2D]):Unit = 
    if (!(buf.size>0 && buf.last==v)) buf += v
    
  /**
   * Clips a polygon with regards to one single edge.
   */
  def clipPolygon(input:IndexedSeq[Vec2D], edge:Line2D ) : IndexedSeq[Vec2D] = {
    val output = new ArrayBuffer[Vec2D] // outputList
      
    (0 until input.size).foreach( i => {
      val inputSize = input.size
      val current = input(i)
      val previous = input((i+inputSize-1)%inputSize)
         
      if (isInside(current, edge)) {
         if (!isInside(previous, edge)) {
           val iPoint = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(edge, previous, current)
           if (iPoint.isDefined) {
             append(iPoint.get, output)
           } else {
             println(" No intersection found: edge=" + edge + " previous=" + previous + " current=" + current)
           }
         }
         append(current, output)
      } else if (isInside(previous, edge)) {
        val iPoint = SutherlandHodgemanClipper.getIntersectionPosOnInfiniteLine(edge, previous, current)
        if (iPoint.isDefined) {
          append(iPoint.get, output)
        } else {
          println(" No intersection found: edge=" + edge + " previous=" + previous + " current=" + current)
        }
      }
    })
    
    output
  }
}

object SutherlandHodgemanClipper {
  
  val epsilon = 0.00001f
  
  def intersection(a:Vec2D, b:Vec2D, p:Vec2D, q:Vec2D) : Vec2D = {
    val A1 = b.y.toDouble - a.y.toDouble
    val B1 = a.x.toDouble - b.x.toDouble
    val C1 = A1 * a.x.toDouble + B1 * a.y.toDouble
 
    val A2 = q.y.toDouble - p.y.toDouble
    val B2 = p.x.toDouble - q.x.toDouble
    val C2 = A2 * p.x.toDouble + B2 * p.y.toDouble
 
    val det = A1 * B2 - A2 * B1
    val x = (B2 * C1 - B1 * C2) / det
    val y = (A1 * C2 - A2 * C1) / det
 
    return new Vec2D(x.toFloat, y.toFloat)
  }
  
  /**
   * Returns the intersection point between an infinite line and one finite line (segment)
   */
  def getIntersectionPosOnInfiniteLine(infiniteLine:Line2D, fromV:Vec2D, toV:Vec2D): Option[Vec2D] = {
    val p = getIntersectionPosOnInfiniteLineReal(infiniteLine,fromV, toV)
    if (p.isDefined) {
      val p2 = intersection(infiniteLine.a, infiniteLine.b, fromV, toV)
      if (math.abs(p.get.x - p2.x) < epsilon && math.abs(p.get.y - p2.y) < epsilon) {
        p
      } else {
        println("!##differ!" + p.get + " != " + p2)
        p
      }
    } else {
      //val p2 = intersection(infiniteLine.a, infiniteLine.b, fromV, toV)
      //println("no intersection found " + infiniteLine.a + "->" + infiniteLine.b + "  " + fromV + "->" + toV + "\nbut 'intersection' found: " + p2)
      p 
    }
  }
  
  /**
   * Returns the intersection point between an infinite line and one finite line (segment)
   */
  def getIntersectionPosOnInfiniteLineReal(infiniteLine:Line2D, fromV:Vec2D, toV:Vec2D): Option[Vec2D] = {
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
          Some(toV) // COINCIDENT. Any point on edge is intersecting - what to return?
        } else {
          None // COINCIDENT_NO_INTERSECT
        }
      } else {
        None // PARALLEL
      }
    }
  }  
}

object Test extends App {
  
  val p0 = new Vec2D(1,-1)
  val p1 = new Vec2D(-1,-1)
  val p2 = new Vec2D(-1,1)
  val p3 = new Vec2D(1,1)
  
  var polygon:IndexedSeq[Vec2D] = Array(p0.scale(10), p1.scale(10), p2.scale(10), p3.scale(10))
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
  println("p0: " + clipper.isInside(p0, iLine))
  println("p1: " + clipper.isInside(p1, iLine))
  println("p2: " + clipper.isInside(p2, iLine))
  println("p3: " + clipper.isInside(p3, iLine))
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