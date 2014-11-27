package org.toxicblend.geometry

import toxi.geom.Vec2D
import toxi.geom.ReadonlyVec2D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D

object ProjectionPlane extends Enumeration {
    type ProjectionPlane = Value
    val YZ_PLANE,XZ_PLANE,XY_PLANE = Value
    
    @inline
    def convert(projectionPlane:ProjectionPlane, vertex:ReadonlyVec2D ) = {
      projectionPlane match {
	      case ProjectionPlane.YZ_PLANE => new Vec3D(0f,       vertex.x, vertex.y)
	      case ProjectionPlane.XZ_PLANE => new Vec3D(vertex.x,       0f, vertex.y)
	      case ProjectionPlane.XY_PLANE => new Vec3D(vertex.x, vertex.y,       0f)
	    }
    }
    
    @inline
    def convert(projectionPlane:ProjectionPlane, vertex:ReadonlyVec3D ) = {
      projectionPlane match {
	      case ProjectionPlane.YZ_PLANE => new Vec2D(vertex.y, vertex.z)
	      case ProjectionPlane.XZ_PLANE => new Vec2D(vertex.x, vertex.z)
	      case ProjectionPlane.XY_PLANE => new Vec2D(vertex.x, vertex.y)
	    }
    }
}