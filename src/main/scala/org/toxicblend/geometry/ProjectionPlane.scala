package org.toxicblend.geometry

import org.toxicblend.vecmath.Vec2D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D

object ProjectionPlane extends Enumeration {
    type ProjectionPlane = Value
    val YZ_PLANE,XZ_PLANE,XY_PLANE = Value
    
    @inline
    def convert(projectionPlane:ProjectionPlane, vertex:Vec2D ): Vec3D = {
      projectionPlane match {
	      case ProjectionPlane.YZ_PLANE => new Vec3D(0f,               vertex.x.toFloat, vertex.y.toFloat)
	      case ProjectionPlane.XZ_PLANE => new Vec3D(vertex.x.toFloat,               0f, vertex.y.toFloat)
	      case ProjectionPlane.XY_PLANE => new Vec3D(vertex.x.toFloat, vertex.y.toFloat,               0f)
	    }
    }
    
    @inline
    def convert(projectionPlane:ProjectionPlane, vertex:ReadonlyVec3D ):Vec2D = {
      projectionPlane match {
	      case ProjectionPlane.YZ_PLANE => Vec2D(vertex.y, vertex.z)
	      case ProjectionPlane.XZ_PLANE => Vec2D(vertex.x, vertex.z)
	      case ProjectionPlane.XY_PLANE => Vec2D(vertex.x, vertex.y)
	    }
    }
}