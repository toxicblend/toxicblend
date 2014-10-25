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
  def distanceToSquared(v:Vec2D): Double
  def distanceToSquared(vx:Double, vy:Double):Double
  def heading:Double
  def interpolateTo(v:Vec2D, f:Double): Vec2D
  def toIntString:String
  def canEqual(other: Any):Boolean
}