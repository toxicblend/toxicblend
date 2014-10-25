package org.toxicblend.operations.meshgenerator.vecmath

trait Vertex2D {
  def x:Double
  def y:Double
  def interpolateTo(v:Vertex2D):Vertex2D
  def scale(s:Double):Vertex2D
  def add(xp:Double, yp:Double):Vertex2D
  def add(v:Vertex2D):Vertex2D
  def =~=(v:Vertex2D,p:Double):Boolean
  def distanceToSquared(v:Vertex2D):Double
  def distanceToSquared(vx:Double, vy:Double):Double
  def heading:Double
  def interpolateTo(v:Vertex2D, f:Double):Vertex2D
  def toIntString:String
  def canEqual(other: Any):Boolean
}