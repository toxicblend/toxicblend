package org.toxicblend.geometry

import toxi.geom.Matrix4x4

object Matrix4x4Implicit {
  implicit def Matrix4x4Extension(m:Matrix4x4) = new Matrix4x4Extension(m)
}