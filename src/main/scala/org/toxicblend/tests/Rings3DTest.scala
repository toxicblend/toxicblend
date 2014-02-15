package org.toxicblend.tests

import org.toxicblend.geometry.Rings2D
import scala.collection.mutable.ArrayBuffer
import toxi.geom.ReadonlyVec2D
import toxi.geom.Vec2D

object Rings3DTest {

  def main(args: Array[String]): Unit = {
     //Rings2D(vertices:ArrayBuffer[ReadonlyVec2D], faces:ArrayBuffer[ArrayBuffer[Int]])  
    
    val vertices = new ArrayBuffer[ReadonlyVec2D]
    vertices += new Vec2D(10,10)
    vertices += new Vec2D(10,-10)
    vertices += new Vec2D(-10,-10)
    vertices += new Vec2D(-10,10)
    vertices += new Vec2D(10,10)
    vertices += new Vec2D(10,-10)
    vertices += new Vec2D(-10,-10)
    vertices += new Vec2D(-10,10)
    vertices += new Vec2D(10,10)
    vertices += new Vec2D(10,-10)
    vertices += new Vec2D(-10,-10)
    vertices += new Vec2D(-10,10)
    vertices += new Vec2D(10,10)
    vertices += new Vec2D(10,-10)
    vertices += new Vec2D(-10,-10)
    vertices += new Vec2D(-10,10)
    
    val edges = new ArrayBuffer[ArrayBuffer[Int]]
    edges += ArrayBuffer[Int](0,1)
    edges += ArrayBuffer[Int](1,2)
    edges += ArrayBuffer[Int](2,3)
    edges += ArrayBuffer[Int](3,0)
   
    edges += ArrayBuffer[Int](10,11)
    edges += ArrayBuffer[Int](11,12)
    edges += ArrayBuffer[Int](12,10)

    val ring = new Rings2D(vertices,edges)
    println(ring.toString())
  }
}