package org.toxicblend.operations.zadjust.jbullet
import javax.vecmath.Vector3f
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D

/**
 * Simple conversion utilities
 */
object JBulletUtil {
  /**
   * copy a Vector3f into a new Vec3D
   */
  @inline
  def convertVector3fToVec3D(input:Vector3f):Vec3D = {
    new Vec3D(input.x, input.y, input.z)
  }
  
  /**
   * copy the values of a Vector3f into a Vec3D
   */
  @inline
  def copyVector3fToVec3D(result:Vec3D, input:Vector3f):Vec3D = {
    result.x = input.x
    result.y = input.y
    result.z = input.z
    result
  }
  
  /**
   * copy a ReadonlyVec3D into a new Vector3f
   */
  @inline
  def convertVec3DToVector3f(input:ReadonlyVec3D):Vector3f = {
    new Vector3f(input.x, input.y, input.z)
  }
  
  /**
   * copy the values of a Vec3D into a Vector3f
   */
  @inline
  def copyVec3DToVector3f(result:Vector3f, input:Vec3D):Vector3f = {
    result.x = input.x
    result.y = input.y
    result.z = input.z
    result
  }
}