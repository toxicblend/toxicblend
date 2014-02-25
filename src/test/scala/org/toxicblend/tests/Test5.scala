package org.toxicblend.tests

import org.toxicblend.operations.boostmedianaxis.MedianAxisJni
import toxi.geom.Vec2D
import toxi.geom.Vec3D
import org.toxicblend.geometry.Matrix3x3

object Test5 {

  def main(args: Array[String]): Unit = {
    val v = new Vec2D(10,11)
    val t = new Vec2D(0,0)
    val m = new Matrix3x3(10,t)
    val mi = m.copy.inverse
    println(m)
    println(mi)
    println(v)
    m.transform2D(v)
    println(v)
    mi.transform2D(v)
    println(v)
    println(m)
    println(mi)
    
    if (false) {
      println("simplifyLimit:")
  
      val ma = MedianAxisJni()
      val input = Array(new Vec2D(0,0), new Vec2D(5,5), new Vec2D(10,10), new Vec2D(10,20))
      val ringId = ma.addRing(input, 5.0f)
      println("getRing:" + ma.getRing(ringId).mkString(","))
      
      val output = MedianAxisJni.simplify2DSingle(input, 1.0f)
      println(input.mkString(","))
      println(output.mkString(","))
      ma.destructor
    }
  }
}

/*
  
  
 



val FloatRegEx="(\\d+(\\.\\d+)?)".r
def getValue(s:String) :Float = s match {
  case FloatRegEx(num) => num.toFloat
  case _ => 0.0f
}





 */
