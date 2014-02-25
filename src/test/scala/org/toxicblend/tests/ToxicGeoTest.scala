package org.toxicblend.tests

import toxi.geom.Vec2D
import toxi.geom.Vec3D
import toxi.geom.Rect
import org.toxicblend.geometry.Matrix3x3

object ToxicGeoTest {

  def main(args: Array[String]): Unit = {
    if(false) {
      val v = new Vec2D(10,11)
      val t = new Vec2D(0,0)
      val m = new Matrix3x3()
      
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
    } else if (false){
      
      val m = new Matrix3x3(2,  new Vec2D(100,100))
      val p = new Vec2D(200,300)
      println(p)
      m.transform2D(p)
      println(p)
      
      val mi = m.copy.inverse
      mi.transform2D(p)
      println("m=" + m)
      println("mi=" + mi)
      println(p)

    } else {
      val (srcX, srcY, srcW, srcH) = (0,0,300,300)
      val srcBB = new Rect(srcX, srcY, srcW, srcH)
      val dstBB = new Rect(-1000,-1000,2000,2000)
      println("srcBounds:" + srcBB)
      println("dstBB:" + dstBB)

      val m = Matrix3x3.getTranslation(srcBB,dstBB)
      println(m)
      val mi = m.copy.inverse
      val p1 = new Vec2D(srcX,srcY)
      val p2 = new Vec2D(srcX+srcW/2,srcY+srcH/2)
      val p3 = new Vec2D(srcX+srcW,srcY+srcH)
      
      println("p1:\t" + p1 + " \tp2:" + p2 + " \tp3:" + p3)
      m.transform2D(p1); m.transform2D(p2); m.transform2D(p3)
      println("p1:\t" + p1 + " \tp2:" + p2 + " \tp3:" + p3)
      mi.transform2D(p1); mi.transform2D(p2); mi.transform2D(p3)
      println("p1:\t" + p1 + " \tp2:" + p2 + " \tp3:" + p3)
    }
  }
}