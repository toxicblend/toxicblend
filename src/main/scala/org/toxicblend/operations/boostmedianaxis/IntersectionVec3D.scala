package org.toxicblend.operations.boostmedianaxis

import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D

/**
 * @deprecated??
 */
class IntersectionVec3D(inX:Float,inY:Float,inZ:Float) extends Vec3D(inX,inY,inZ) {
  def this() = this(0f,0f,0f)
  def this(v:Vec3D) = this(v.x, v.y, v.z)
  
  /** 
   * does the line segment defined by 'this' to p1 intersect the XY plane where Z=zlimit? 
   * TODO: figure out when to use >= and <=
   */
  def intersectsXYPlan_e(p1:ReadonlyVec3D, zLimit:Float):Boolean = {
    if ((this.z == p1.z) || 
        (this.z > zLimit && p1.z > zLimit) || 
        (this.z < zLimit && p1.z < zLimit)){
      false
    } else
    if (this.z >= zLimit){
      p1.z < zLimit
    } else if (p1.z >= zLimit){
      this.z < zLimit
    } else {
     false
    }
  }
  
  def intersectsXYPlane(p1:ReadonlyVec3D, zLimit:Float):Boolean = {
    var rv = intersectsXYPlan_e(p1, zLimit)
    //println("comparing this.z %f that.z %f : %s".format(this.z, p1.z, rv.toString))
    rv
  }
 
  def intersectoPoint(p1:ReadonlyVec3D, zLimit:Float):IntersectionVec3D = {
    val t = (z - zLimit)/(p1.z-z)
    new IntersectionVec3D((x -(p1.x -x)*t), 
              (y -(p1.y -y)*t),
              (z -(p1.z -z)*t))
  }
  
}