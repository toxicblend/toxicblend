package org.toxicblend.geometry

import toxi.geom.ReadonlyVec3D
import toxi.geom.Line3D
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

/**
 * Ported from JTS (http://www.vividsolutions.com/jts/JTSHome.htm)
 * LGPL License
 */
class LineSegment (var p0:ReadonlyVec3D, var p1:ReadonlyVec3D) {
  
  @inline protected def crossSquared(v0:ReadonlyVec3D, v1:ReadonlyVec3D) = {
    v0.cross(v1).magSquared
  }
  
  def distanceSquared(p:ReadonlyVec3D):Float = {
    val d = p1.sub(p0).magSquared
    if (d==0) {
      p0.distanceToSquared(p)
    } else {
      val n = crossSquared( p1.sub(p0), p0.sub(p))
      n/d
    }
  }
}

/**
 * Simplifies a sequence of points using
 * the standard Ramer-Douglas-Peucker algorithm.
 */
class RamerDouglasPeuckerAlgorithm(private val pts:IndexedSeq[ReadonlyVec3D], private var distanceToleranceSquared:Float=0.1f)
{

  private val usePt = ArrayBuffer.fill(pts.size)(true)
  private val seg = new LineSegment(pts(0), pts(1))
  
  /**
   * Sets the distance tolerance for the simplification.
   * All vertices in the simplified linestring will be within this
   * distance of the original linestring.
   *
   * @param distanceTolerance the approximation tolerance to use
   */
  def setDistanceTolerance(distanceToleranceSquared:Float) = {
    this.distanceToleranceSquared = distanceToleranceSquared;
  }

  def simplify: IndexedSeq[ReadonlyVec3D] = {
    simplifySection(0, pts.length - 1);
    //CoordinateList coordList = new CoordinateList();
    val coordList = pts.zip(usePt).withFilter(_._2).map(_._1)
    coordList
  }

  private def simplifySection(i:Int, j:Int):Unit = {
    if((i+1) == j) {
      return
    }
    seg.p0 = pts(i)
    seg.p1 = pts(j)
    var maxDistance = -1.0
    var maxIndex = i;
    (i+1 until j).foreach(k=>{
      val distance = seg.distanceSquared(pts(k))
      if (distance > maxDistance) {
        maxDistance = distance;
        maxIndex = k;
      }
    })
    
    if (maxDistance <= distanceToleranceSquared) {
      (i+1 until j).foreach(k => usePt(k) = false)
    } else {
      simplifySection(i, maxIndex);
      simplifySection(maxIndex, j);
    }
  }
}

object RamerDouglasPeuckerAlgorithm {
  def simplify(pts:IndexedSeq[ReadonlyVec3D], distanceTolerance:Float):  IndexedSeq[ReadonlyVec3D] = {
    val algo = new RamerDouglasPeuckerAlgorithm(pts, distanceTolerance*distanceTolerance)
    return algo.simplify
  }
}
