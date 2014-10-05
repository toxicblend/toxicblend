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
           output.add(getClippedPosOnEdge(edge, previous, current).get) 
           //println("\tadd clip")
         }
         output.add(current)
         //println("\tadd current")
      } else if (previousInside) {
        output.add(getClippedPosOnEdge(edge, previous, current).get)
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
  
  def toRad(angle:Float) = (math.Pi * angle / 180f).toFloat
  def cos(angle:Float) = math.cos(angle).toFloat
  def sin(angle:Float) = math.sin(angle).toFloat
  def vec2dpolar(mag:Float,rad:Float) = new Vec2D(mag*cos(rad),mag*sin(rad)) 
    
  val origin = new Vec2D(0,0)
  val edge = new Line2D(new Vec2D(-1,-1f).addSelf(origin), new Vec2D(2,2).addSelf(origin))
  val p0 = new Vec2D(0,0).addSelf(origin)
  val p1 = new Vec2D(1,0).addSelf(origin)
  val p2 = new Vec2D(0,1).addSelf(origin)
  val poly = new Polygon2D
  poly.add(p0); poly.add(p1); poly.add(p2);
  val center = new Vec2D(0f,0f)
  val clipper = new SutherlandHodgemanClipper
  val alpha = 180f + 45f
  val alphaDelta = -360f/100f
  val sign = clipper.clockwiseFactor(edge)
  
  if (true) for (i <- 0 until 50) {
    val pos = vec2dpolar(0.5f,toRad(alpha+alphaDelta*i)).addSelf(origin)
    if (clipper.isInside(pos, edge, sign)) {
      println("outside fail: angle=" + (alpha+alphaDelta*i) + " pos=" + pos)
    }
  }
  for (i <- 50 until 100) {
    val pos = vec2dpolar(0.5f,toRad(alpha+alphaDelta*i)).addSelf(origin)
    if (!clipper.isInside(pos, edge, sign)) {
      println("inside fail: angle=" + (alpha+alphaDelta*i) + " pos=" + pos)
    }
  }
  println("unclipped: " + poly)
  println("p0: " + p0 + " isInside:" + clipper.isInside(p0, edge, sign))
  println("p1: " + p1 + " isInside:" + clipper.isInside(p1, edge, sign))
  println("p2: " + p2 + " isInside:" + clipper.isInside(p2, edge, sign))

    
  val clipped = clipper.clipPolygon(poly,edge)
  println("clipped:" + clipped)
  println("end")
}