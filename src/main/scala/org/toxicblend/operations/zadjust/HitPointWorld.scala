package org.toxicblend.operations.zadjust
import com.bulletphysics.linearmath.Point3dE

class HitPointWorld {
  val collisionPoint = new Point3dE
  var triangleIndex:Int = -1
  
  @inline def hasTriangleIndex=triangleIndex>=0
  
  override def toString() = {
    "collisionPoint:" + collisionPoint + " triangleIndex:" + triangleIndex 
  } 
}