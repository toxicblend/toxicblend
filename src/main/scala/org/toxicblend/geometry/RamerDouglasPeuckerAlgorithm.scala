package org.toxicblend.geometry

import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.Line3D
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

/**
 * Ramer-Douglas-Peucker algorithm implementation ported from JTS (http://www.vividsolutions.com/jts/JTSHome.htm)
 * LGPL License
 *
 * Simplifies a sequence of points using
 * the standard Ramer-Douglas-Peucker algorithm.
 */
class RamerDouglasPeuckerAlgorithm(private val pts:IndexedSeq[ReadonlyVec3D], private var distanceToleranceSquared:Float=0.1f)
{

  private val usePt = ArrayBuffer.fill(pts.size)(true)
  private val seg = new QuickLineSegment(pts(0), pts(1))
  
  /**
   * Sets the distance tolerance for the simplification.
   * All vertices in the simplified linestring will be within this
   * distance of the original linestring.
   *
   * @param distanceTolerance the approximation tolerance to use
   */
  def setDistanceTolerance(distanceToleranceSquared:Float) = {
    this.distanceToleranceSquared = distanceToleranceSquared
    (0 until usePt.size).foreach(i=>usePt(i)=true)
  }

  def simplify: IndexedSeq[ReadonlyVec3D] = {
    simplifySection(0, pts.length - 1);
    //CoordinateList coordList = new CoordinateList();
    assert(usePt(0) == true)
    assert(usePt(usePt.size-1) == true)
    assert(usePt.size == pts.size)
    val coordList = pts.zip(usePt).withFilter(_._2).map(_._1.copy)
    coordList
  }

  private def simplifySection(i:Int, j:Int):Unit = {
    if((i+1) == j) {
      return
    }
    seg.p0 = pts(i)
    seg.p1 = pts(j)
    var maxDistance = -1.0f
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
  def simplify(pts:IndexedSeq[ReadonlyVec3D], distanceTolerance:Float): IndexedSeq[ReadonlyVec3D] = {
    //println("distanceTolerance: " + distanceTolerance)
    //println("Input:  " + pts.mkString(","))
    val algo = new RamerDouglasPeuckerAlgorithm(pts, distanceTolerance*distanceTolerance)
    val rv = algo.simplify
    //println("Output: " + rv.mkString(","))
    rv
  }
}
