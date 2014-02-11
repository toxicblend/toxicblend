package org.toxicblend.geometry

import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D

/**
 * Add a few z-plane-comparison methods to the toxi.geom.Vec3D
 */
class IntersectionVec3D(val wrapped:ReadonlyVec3D) {
 
  /** 
   * does the line segment defined by 'this' to p1 intersect the XY plane where Z=zlimit? 
   * TODO: figure out when to use >= and <=
   */
  def intersectsXYPlane_(p1:ReadonlyVec3D, zLimit:Float):Boolean = {
    if ((wrapped.z == p1.z) || 
        (wrapped.z > zLimit && p1.z > zLimit) || 
        (wrapped.z < zLimit && p1.z < zLimit)){
      false
    } else
    if (wrapped.z >= zLimit){
      p1.z <= zLimit
    } else if (p1.z >= zLimit){
      wrapped.z <= zLimit
    } else {
     false
    }
  }
  
  def intersectsXYPlane(p1:ReadonlyVec3D, zLimit:Float):Boolean = {
    var rv = intersectsXYPlane_(p1, zLimit)
    println("comparing this.z %f that.z %f zLimit=%f: %s".format(wrapped.z, p1.z, zLimit, rv.toString))
    rv
  }
 
  def intersectionPoint(p1:ReadonlyVec3D, zLimit:Float):Vec3D = {
    val t = (wrapped.z - zLimit)/(p1.z-wrapped.z)
    new Vec3D((wrapped.x -(p1.x -wrapped.x)*t), 
              (wrapped.y -(p1.y -wrapped.y)*t),
              (wrapped.z -(p1.z -wrapped.z)*t))
  }
}
