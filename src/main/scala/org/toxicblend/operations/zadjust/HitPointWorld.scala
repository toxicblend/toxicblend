package org.toxicblend.operations.zadjust
import com.bulletphysics.linearmath.Point3dE

class HitPointWorld {
  val point = new Point3dE
  var triangleIndex:Int = -1
  def copyHitpoint = new Point3dE(point)
}