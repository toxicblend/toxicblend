package org.toxicblend.geometry
import toxi.geom.Matrix4x4

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
}