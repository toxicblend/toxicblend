package org.toxicblend.operations.zadjust.jbullet
import javax.vecmath.Vector3d
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D

/**
 * Simple conversion utilities
 */
object JBulletUtil {
  
   /**
    * returns the squared distance between two vertices in XY plane ( Z coordinate is ignored )
    */
   @inline
   def sqrXYDistance(v0:ReadonlyVec3D, v1:ReadonlyVec3D) = {
     val dx = (v0.x - v1.x)//.toDouble
     val dy = (v0.y - v1.y)//.toDouble
     dx*dx + dy*dy   
   }
  
   /**
    * returns the squared distance between two vertices in XY plane ( Z coordinate is ignored )
    */
   @inline
   def sqrXYDistance(v0:ReadonlyVec3D, v1:Vector3d) = {
     val dx = (v0.x - v1.x)//.toDouble
     val dy = (v0.y - v1.y)//.toDouble
     dx*dx + dy*dy   
   }
   
  /**
   * copy a Vector3d into a new Vec3D
   */
  @inline
  def vector3dToNewVec3D(input:Vector3d) = new Vec3D(input.x.toFloat, input.y.toFloat, input.z.toFloat)
  
  /**
   * copy the values of a Vector3d into a Vec3D
   */
  @inline
  def copyVector3dToVec3D(result:Vec3D, input:Vector3d):Vec3D = {
    result.x = input.x.toFloat
    result.y = input.y.toFloat
    result.z = input.z.toFloat
    result
  }
  
  /**
   * copy a ReadonlyVec3D into a new Vector3d
   */
  @inline
  def vec3DToNewVector3d(input:ReadonlyVec3D) = new Vector3d(input.x, input.y, input.z)
  
  /**
   * copy the values of a Vec3D into a Vector3d
   */
  @inline
  def copyVec3DToVector3d(result:Vector3d, input:Vec3D):Vector3d = {
    result.x = input.x
    result.y = input.y
    result.z = input.z
    result
  }
}