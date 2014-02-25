package org.toxicblend.geometry
import toxi.geom.Matrix4x4
import toxi.geom.ReadonlyVec3D

class Matrix4x4Extension( __v11:Double, __v12:Double, __v13:Double, __v14:Double,
                          __v21:Double, __v22:Double, __v23:Double, __v24:Double,  
                          __v31:Double, __v32:Double, __v33:Double, __v34:Double,
                          __v41:Double, __v42:Double, __v43:Double, __v44:Double) 
             extends Matrix4x4(__v11, __v12, __v13, __v14,
                               __v21, __v22, __v23, __v24,
                               __v31, __v32, __v33, __v34,
                               __v41, __v42, __v43, __v44) {
  
  /**
   * Deep copy constructor
   */
  def this(__ext:Matrix4x4) = this(__ext.matrix(0)(0), __ext.matrix(0)(1), __ext.matrix(0)(2), __ext.matrix(0)(3),
                                   __ext.matrix(1)(0), __ext.matrix(1)(1), __ext.matrix(1)(2), __ext.matrix(1)(3),
                                   __ext.matrix(2)(0), __ext.matrix(2)(1), __ext.matrix(2)(2), __ext.matrix(2)(3),
                                   __ext.matrix(3)(0), __ext.matrix(3)(1), __ext.matrix(3)(2), __ext.matrix(3)(3))
  
  /**
   * Constructs this matrix as a scale and translate matrix 
   */                                 
  def this(__trans:ReadonlyVec3D, __scale:ReadonlyVec3D) = 
    this(__scale.x,         0,         0, __trans.x,
                 0, __scale.y,         0, __trans.y,
                 0,         0, __scale.z, __trans.z,
                 0,         0,         0,         1)    
                 
  /**
   * Constructs this matrix as a identity matrix 
   */                                 
  def this() = this(1,0,0,0,
                    0,1,0,0,
                    0,0,1,0,
                    0,0,0,1)    
                             
  /**
   * converts this matrix into the identity matrix (in place)
   */
  def setIdentity:Matrix4x4Extension = {
    (0 until 4).foreach(i=>{
      val v = matrix(i)
      (0 until 4).foreach(j=>{
        if (i==j){
          v(j) = 1
        } else {
          v(j) = 0
        }
      })
    })
    this
  }
  
  /**
   * converts this matrix into a scale and translate matrix (in place)
   */
  def translateScale(translate:ReadonlyVec3D, scale:ReadonlyVec3D):Matrix4x4Extension = {
    this.set( scale.x,       0,       0, translate.x, 
                    0, scale.y,       0, translate.y,
                    0,       0, scale.z, translate.z,
                    0,       0,       0,           1)
            
    this
  }
}