package org.toxicblend.geometry

import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.Line3D

class QuickLineSegment(var p0: ReadonlyVec3D, var p1: ReadonlyVec3D) {

  @inline def squaredDistanceToProjection(p: ReadonlyVec3D, t: Float) = {
    val vx = p0.x + t * (p1.x - p0.x) - p.x
    val vy = p0.y + t * (p1.y - p0.y) - p.y
    val vz = p0.z + t * (p1.z - p0.z) - p.z
    vx * vx + vy * vy + vz * vz
  }

  /**
   * Calculates the squared distance from this segment to a point.
   * It tries to do the same thing as:
   *   new toxi.geom.Line3D(p0, p1).closestPointTo(p).subSelf(p).magSquared
   * but without creating any new objects.
   *
   * It turns out that the "findContinuousLineSegments()" method spends ten times
   * more CPU cycles than the RamerDouglasPeuckerAlgorithm implementation.
   * And that the speedup from the "toxiclib" version of this method is barely noticable.
   *
   * Can i get a "premature optimization" from anyone? :P
   */
  def distanceSquared(p: ReadonlyVec3D): Float = {
    val segmentLengthSquared = QuickLineSegment.subLengthSquared(p0, p1)
    if (segmentLengthSquared == 0) {
      p0.distanceToSquared(p) // p0 == p1
    } else {
      val t = QuickLineSegment.dot(p.x - p0.x, p.y - p0.y, p.z - p0.z, p1.x - p0.x, p1.y - p0.y, p1.z - p0.z) / segmentLengthSquared
      if (t <= 0)
        QuickLineSegment.subLengthSquared(p0, p)
      else if (t >= 1)
        QuickLineSegment.subLengthSquared(p1, p)
      else
        squaredDistanceToProjection(p, t)
    }
  }

  def distanceSquaredToxic(p: ReadonlyVec3D): Float = {
    (new Line3D(p0, p1)).closestPointTo(p).subSelf(p).magSquared
  }
}

object QuickLineSegment {

  /**
   * calculates the dot product of the vectors (ux,uy,uz) and  (vx,vy,vz)
   */
  @inline protected def dot(ux: Float, uy: Float, uz: Float, vx: Float, vy: Float, vz: Float) = {
    val rv = ux * vx + uy * vy + uz * vz
    //assert(rv == new Vec3D(ux,uy,uz).dot(new Vec3D(vx,vy,vz)))
    rv
  }

  /**
   * Calculates the squared length of an subtracted vector without
   * actually creating the vector
   */
  @inline protected def subLengthSquared(v0: ReadonlyVec3D, v1: ReadonlyVec3D) = {
    val sx = v0.x - v1.x
    val sy = v0.y - v1.y
    val sz = v0.z - v1.z
    val subLengthSquared = sx * sx + sy * sy + sz * sz
    //assert(subLengthSquared == v0.sub(v1).magSquared )
    subLengthSquared
  }

  /**
   * Calculates the squared length of an added vector without
   * actually creating the vector
   */
  @inline protected def addLengthSquared(v0: ReadonlyVec3D, v1: ReadonlyVec3D) = {
    val sx = v0.x + v1.x
    val sy = v0.y + v1.y
    val sz = v0.z + v1.z
    val subLengthSquared = sx * sx + sy * sy + sz * sz
    //assert(subLengthSquared == v0.add(v1).magSquared )
    subLengthSquared
  }

  /**
   * Calculates the squared length of a cross product without
   * actually creating the cross product vector
   */
  @inline protected def crossLengthSquared(ux: Float, uy: Float, uz: Float, vx: Float, vy: Float, vz: Float) = {
    val cx = uy * vz - uz * vy
    val cy = uz * vx - ux * vz
    val cz = ux * vy - uy * vx
    val crossLengthSquared = cx * cx + cy * cy + cz * cz
    //assert(crossLengthSquared == new Vec3D(ux, uy, uz).cross( new Vec3D(vx, vy, vz) ).magSquared)
    crossLengthSquared
  }
}