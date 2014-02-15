package org.toxicblend.tests

import org.toxicblend.geometry.Mesh2D
import toxi.geom.Vec2D
import toxi.geom.ReadonlyVec2D
import scala.collection.mutable.ArrayBuffer
import org.toxicblend.typeconverters.SeqShift

object Test2 {
  def mTestData = {
    val v = new ArrayBuffer[ReadonlyVec2D]()
    val f = new ArrayBuffer[ArrayBuffer[Int]]()
    v += new Vec2D(10,10)//,10)
    v += new Vec2D(10,-10)//,10)
    v += new Vec2D(-10,-10)//,10)
    v += new Vec2D(-10,10)//,10)
    f += ArrayBuffer(0,1,2,3)
    
    v += new Vec2D(11,10)//,-10)
    v += new Vec2D(12,-10)//,-10)
    v += new Vec2D(-13,-10)//,-10)
    v += new Vec2D(-14,10)//,-10)
    f += ArrayBuffer(4,5,6,7)
    
    f += ArrayBuffer(0,1,5,4)
    f += ArrayBuffer(1,5,6,2)
    f += ArrayBuffer(2,6,7,3)
    f += ArrayBuffer(0,4,7,3)
    (v, f)
  }
  
  def main(args: Array[String]): Unit = {
     
    val (v,f) = mTestData
    val mc = Mesh2D(v,f)
    println(mc.vertices.mkString(","))
    println(mc.faces.map(x => x.mkString(",")).mkString(" : "))
    //SeqShift.rotate(mc.faces(1), -2)
    //println(mc.faces.map(x => x.mkString(",")).mkString(" : "))
  }
}