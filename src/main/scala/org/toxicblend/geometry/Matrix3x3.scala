package org.toxicblend.geometry

import toxi.geom.Matrix3d
import toxi.geom.Vec2D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Rect

/**
 * Basically the same as toxi.geom.Matrix3d, just added some convenience methods
 */
class Matrix3x3(m̈00:Double,m̈01:Double,m̈02:Double,m̈10:Double,m̈11:Double,m̈12:Double,m̈20:Double,m̈21:Double,m̈22:Double)  
  extends Matrix3d (m̈00,m̈01,m̈02,m̈10,m̈11,m̈12,m̈20,m̈21,m̈22) {
  
  def this(scale:Double, translate:ReadonlyVec2D) = {
    this (scale,    0, translate.x, 
          0,    scale, translate.y,
          0,        0,           1)
  }
  
  /**
   * Indentity matrix as default 
   */
  def this() = this(1,0,0, 0,1,0, 0,0,1)
 
  /**
   * Transforms the point parameter with this Matrix3x3 and places the result
   * back into point. The third element of the point input parameter is
   * assumed to be one.
   * 
   * @param point
   *            the input point to be transformed.
   */
  def transform2D(point:Vec2D):Vec2D = {
    //println("tr(" + m00 + "," + m01+ "," +m02+ "),(" +m10+ "," +m11+ "," +m12+")")
    //println("tr(" + m00 + "," + m01+ "," +m02+ "),(" +m10+ "," +m11+ "," +m12+")")
    val x = m00 * point.x + m01 * point.y + m02
    val y = m10 * point.x + m11 * point.y + m12
    point.x = x.toFloat
    point.y = y.toFloat
    point
  }
   
  def copy:Matrix3x3 = {
    new Matrix3x3(m00, m01, m02, m10, m11, m12, m20, m21, m22)
  }
  
  def inverse:Matrix3x3 = {
    invert
    this
  }
}

object Matrix3x3 {
  /**
   * calculates an affine transform that can convert a coordinate, inside the bounding rectangle 'srcBounds', 
   * to a correlating coordinate in the 'dstBounds' bounding rectangle.
   * No rotations and the scaling is symmetrical (same for x & y).
   * 
   * @param srcBounds the source bounding rectangle
   * @param dstBounds the destination bounding rectangle 
   */
  def getTranslation(fromBounds:Rect, toBounds:Rect):Matrix3x3 = {
    val scale = {
      val fromDimensions = fromBounds.getDimensions()
      val toDimensions = toBounds.getDimensions()
      scala.math.min(toDimensions.x/fromDimensions.x, toDimensions.y/fromDimensions.y)
    }
    val fromCentroid = fromBounds.getCentroid()
    val toCentroid = toBounds.getCentroid()
    val offset = new Vec2D(toCentroid.x - fromCentroid.x*scale, toCentroid.y - fromCentroid.y*scale)
    //println("scale:" + scale+ " offset:" + offset + " srcBounds:" + srcBounds + " srcCentroid:" + srcCentroid + " dstBounds:" + dstBounds + " dstCentroid:" + dstCentroid )
    new Matrix3x3(scale,offset)
  }
}