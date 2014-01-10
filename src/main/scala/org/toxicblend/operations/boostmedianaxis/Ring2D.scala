package org.toxicblend.operations.boostmedianaxis

import scala.math.max
import scala.math.min
import scala.math.abs
import scala.collection.mutable.ArrayBuffer
import toxi.geom.ReadonlyVec2D
import toxi.geom.Rect

/**
 * @param ïnVerts is named this way because of the way scala handles constructor input parameters (non-var or val). 
 *     If the variable is, by accident, referenced anywhere in this object it will be retained as a 'private final' in bytecode
 */
class Ring2D( val ma:MedianAxisJni, val name:String, ïnVerts:IndexedSeq[ReadonlyVec2D], var subRings:Array[Ring2D], val simplifyLimit:Float) {
  
  val ringId = ma.addRing(ïnVerts, simplifyLimit)
  val verts = ma.getRing(ringId)
  val bb = new Rect
  verts.foreach(p => bb.growToContainPoint(p))
  
  override def toString:String = {
    bb.toString + "\n" + 
    "jdouble vert_1 = "+verts.mkString("{",",","}")+"\n" + 
    "size_t vert_1_size=%d;\n".format(verts.size)
  }
  
  def outerSegments() : Array[Int] = {
    Array(ringId)
  }
  
  def innerSegments() : Array[Int] = {
    subRings.foldLeft(Array[Int]())((b,a) => a.innerSegments() ++ Array(a.ringId) ++ b)
  }
  
  /**
   * returns true if the bounding box of 'that' is completely inside the bounding box of 'this'
   * TODO: this could be faster with some more detailed tests
   */
  def completelyWithinBB(that: Ring2D) : Boolean = {
    assert(this.ma.eq(that.ma), "Can't compare rings created in different MedianAxisJni instances")
    bb.containsPoint(that.bb.getBottomLeft()) &&
    bb.containsPoint(that.bb.getBottomRight()) &&
    bb.containsPoint(that.bb.getTopLeft()) &&
    bb.containsPoint(that.bb.getTopRight())
  }
  
  /**
   * returns true if the vertexes of 'that' ring are inside 'this' ring
   * calls completelyWithinBB first 
   */
  def complatelyContains(that: Ring2D):Boolean = {
    assert(this.ma.eq(that.ma), "Can't compare rings created in different MedianAxisJni instances")
    if (completelyWithinBB(that)) {
      ma.ringContainsAllPoints(this.ringId, that.verts) 
    } else {
      //println("bb sayz ring %d isn't inside ring %d".format(this.ringId, that.ringId))
      false
    }
  }
  
  def ringContainsAllPoints(points: IndexedSeq[ReadonlyVec2D]):Boolean = {
    val testPoints = points.sliding(1,points.length/2).flatten.toArray
    println("ringContainsPoints: points.length = %d, by = %d".format(points.length, 2+2*(points.length/8)) )
    println("ringContainsPoints: " + testPoints.mkString(","))
    val rv = ma.ringContainsAllPoints(ringId, testPoints)
    rv
  }
}

object Ring2D {
  /**
   * figures out if some rings are completely contained inside another ring
   */
  def sortOutInternals(rings:Array[Ring2D]): Array[Ring2D] = {
    val length=rings.length  
    for(i <- 0 until length){
      if (null!=rings(i)){
        for(j <- i+1 until length){
          if (null!=rings(j) && null!=rings(i)) {
            if ( rings(j).complatelyContains(rings(i))) {
              rings(j).subRings=rings(j).subRings :+ rings(i);
              println("Ring %d is inside ring %d".format(rings(i).ringId, rings(j).ringId))
              rings(i) = null
            } else if (rings(i).complatelyContains(rings(j) )) {
              rings(i).subRings=rings(i).subRings :+ rings(j);
              println("Ring %d is inside ring %d".format(rings(j).ringId, rings(i).ringId))
              rings(j) = null
            } else {
              //println("Ring %d is not inside ring %d (or vice versa)".format(rings(i).ringId, rings(j).ringId))
            } 
          }
        }
      }
    }
    
    for (x <- rings.filter(y => null != y)) {
      x.subRings = sortOutInternals( x.subRings)
    }
    val fixedRings = rings.filter(x => null != x)
    /*def printSubrings(ring:Ring2D, indent:String ):String = {
      val indentNew = "%s  ".format(indent)
      "%s %d\n%s".format(indent, ring.ringId, 
        ring.subRings.map(x => printSubrings(x, indentNew)).mkString(""))
    }*/
    //fixedRings.foreach(x=>println(printSubrings(x,"")))
    fixedRings
  }
}