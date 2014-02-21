package org.toxicblend.geometry
import toxi.geom.Matrix4x4
import toxi.geom.ReadonlyVec3D

class Matrix4x4Extension( __extended:Matrix4x4) extends Matrix4x4(__extended) {
  
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
    this.set( scale.x(),         0,         0, translate.x(), 
                      0, scale.y(),         0, translate.y(),
                      0,         0, scale.z(), translate.z(),
                      0,         0,         0,             1)
            
    this
  }
}