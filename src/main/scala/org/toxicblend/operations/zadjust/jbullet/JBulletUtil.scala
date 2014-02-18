package org.toxicblend.operations.zadjust.jbullet
import javax.vecmath.Vector3f
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D

/**
 * Simple convertion utils
 */
object JBulletUtil {
  @inline
  def convertVector3fToVec3D(input:Vector3f):Vec3D = {
    new Vec3D(input.x, input.y, input.z)
  }
  
  @inline
  def convertVec3DToVector3f(input:ReadonlyVec3D):Vector3f = {
    new Vector3f(input.x, input.y, input.z)
  }
}