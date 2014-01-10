package org.toxicblend.geometry

import toxi.geom.{Matrix4f => JMatrix4f}
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D

/**
 * Basically the same as toxi.geom.Matrix4f
 * Just added some convenience methods
 */
class Matrix4f( m̈00:Float, m̈01:Float, m̈02:Float, m̈03:Float, 
                m̈10:Float, m̈11:Float, m̈12:Float, m̈13:Float, 
                m̈20:Float, m̈21:Float, m̈22:Float, m̈23:Float,
                m̈30:Float, m̈31:Float, m̈32:Float, m̈33:Float) 
                extends JMatrix4f(m̈00,m̈01,m̈02,m̈03, m̈10,m̈11,m̈12,m̈13, m̈20,m̈21,m̈22,m̈23, m̈30,m̈31,m̈32,m̈33) {
  /**
   * Constructs and initializes a Matrix4f from the translation,
   * and scale values; the scale is applied only to the rotational components
   * of the matrix (upper 3x3) and not to the translational components.
   * 
   * @param trans
   *            the translational component of the matrix
   * @param scale
   *            the scale value applied to the individual axes
   */
  def this(trans:ReadonlyVec3D, scale:ReadonlyVec3D) = 
    this( scale.x,       0,       0, trans.x, 
                0, scale.y,       0, trans.y,
                0,       0, scale.z, trans.z,
                0,       0,       0,       0)  
  
  /**
   * Constructs and initializes a Matrix4f from the specified 16 element
   * array. this.m00 =v[0], this.m01=v[1], etc.
   * 
   * @param v
   *            the array of length 16 containing in order
   */
  def this(v:Array[Float]) = this(v( 0), v( 1), v( 2), v( 3),
                                  v( 4), v( 5), v( 6), v( 7),
                                  v( 8), v( 9), v(10), v(11),
                                  v(12), v(13), v(14), v(15))

  
  /**
   * Transforms the point parameter with this Matrix4f and places the result
   * back into point. The fourth element of the point input parameter is
   * assumed to be one.
   * 
   * @param point
   *            the input point to be transformed.
   * @return The transformed vector
   */
  def transformOne3D(point:Vec3D):Vec3D = {
    transformOne(point)
    point
  }
  
}