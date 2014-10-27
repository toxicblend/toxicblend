package org.toxicblend.operations.meshgenerator.vecmath

trait Vec2D {
  def x:Double
  def y:Double
  def interpolateTo(v:Vec2D): Vec2D
  def scale(s:Double): Vec2D
  def add(xp:Double, yp:Double): Vec2D
  def add(v:Vec2D): Vec2D
  def sub(xp:Double, yp:Double): Vec2D
  def sub(v:Vec2D): Vec2D
  def =~=(v:Vec2D, p:Double): Boolean
  def magnitude:Double
  def magnitudeSquared:Double
  def distanceTo(vx:Double, vy:Double): Double
  def distanceTo(v:Vec2D): Double
  def distanceToSquared(v:Vec2D): Double
  def distanceToSquared(vx:Double, vy:Double):Double
  def heading:Double
  def interpolateTo(v:Vec2D, f:Double): Vec2D
  def toIntString:String
  def canEqual(other: Any):Boolean
  def copy:Vec2D
}

object Vec2D {
  def apply(x:Int, y:Int) = new ImmutableVec2D(x, y)
  def apply(x:Double, y:Double) = new ImmutableVec2D(x, y)
  def apply() = new ImmutableVec2D(0d, 0d)
}