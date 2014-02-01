package org.toxicblend.geometry
import toxi.geom.ReadonlyVec3D

object IntersectionVec3DImplicit{
  implicit def ReadonlyVec3DToIntersectionVec3D(w:ReadonlyVec3D) = new IntersectionVec3D(w)
}