package org.toxicblend.tests

import java.awt.geom.{AffineTransform => JAffineTransform}
import java.awt.geom.{Point2D => JPoint2D}
import java.awt.geom.{Rectangle2D => JRectangle2D}
import scala.math.abs
import scala.math.min

class Point2D(ẍ:Float, ÿ:Float) extends JPoint2D.Float(ẍ,ÿ) {
  def this() = this(0f,0f)
  def this(p:Point2D) = this(p.getX.toFloat,p.getY.toFloat)
}

class Rectangle2D(x1:Float, y1:Float, x2:Float, y2:Float) extends JRectangle2D.Float(x1,y1,x2,y2) {
  def this() = this(0f,0f,0f,0f)
}

class AffineTransform(flatm̈atrix:Array[Float]) extends JAffineTransform(flatm̈atrix(0),flatm̈atrix(1),flatm̈atrix(2),flatm̈atrix(3),flatm̈atrix(4),flatm̈atrix(5)) {
  
  /**
   * Create unity transform by default
   */
  def this() = this(Array(1f,0f,0f, 1f,0f,0f))
  
  /**
   * Copy constructor
   */
  def this(src:AffineTransform) = 
    this ({
      val mv=new Array[Double](6)
      src.getMatrix(mv)
      mv.map(x => x.toFloat)  
    })
  
  def copy:AffineTransform = {
    new AffineTransform(this)
  }
  
  /**
   * transforms a rectangle into a new one
   */
  def transform(r:Rectangle2D):Rectangle2D = {
    val rv = createTransformedShape(r).getBounds2D()
    new Rectangle2D(rv.getX().toFloat, rv.getY.toFloat,rv.getWidth().toFloat,rv.getWidth().toFloat)
  }
  
  def inverse:AffineTransform = {
    invert
    this
  }
  
  @inline
  def transformSelf(p:Point2D) = transform(p,p)
}

object AwtGeoTest {
  
  /**
   * return a transform that will center the 'sample' inside 'limit' and 
   * scale it up so that one axis of 'sample' correlates to the size of 'limit'
   */
  def getFillTransform(limit:Rectangle2D, sample:Rectangle2D) = {
    val m = new AffineTransform
    //println(m)
    //println(m)
    
    val scale = min(limit.getWidth/sample.getWidth, limit.getHeight/sample.getHeight)
    m.translate(limit.getCenterX, limit.getCenterY)
    m.scale(scale,scale)
    m.translate(-sample.getCenterX, -sample.getCenterY)

    //println("limit.getCenterX:" + limit.getCenterX)
    //println("limit.getCenterY:" + limit.getCenterY)
    
    //println("sample.getCenterX:" + sample.getCenterX)
    //println("sample.getCenterY:" + sample.getCenterY)
    
    m
  }
  
  def aTest={
    val (sampleX, sampleY, sampleW, sampleH)= (0f,0f,300f,300f) 
    val limit = new Rectangle2D(-1000,-1000, 2000, 2000)
    val sample = new Rectangle2D(sampleX,sampleY, sampleW, sampleH)
    
    println("Center = (" +limit.getCenterX + "," + limit.getCenterY + ")")
    //val testm = new AffineTransform
    //testm.setToIdentity()
    //println(testm)
    //val data = new Array[Double](6)
    //testm.getMatrix(data)
    //println(testm)
    //println(data.mkString(","))
    val m = getFillTransform(limit,sample)
    println(m)
    println()
    
    val p1=new Point2D(sampleX,sampleY)
    val p2=new Point2D(sampleX+sampleW/2,sampleY+sampleH/2)
    val p3=new Point2D(sampleX+sampleW,sampleY+sampleH)
    //val p3=new Point2D()
       
    val mi = new AffineTransform(m); mi.invert()
    
    println()
    
    println("p1 " + p1)
    println("p2 " + p2)
    println("p3 " + p3)
    println()
    
    m.transformSelf(p1)
    m.transformSelf(p2)
    m.transformSelf(p3)
    
    println("p1 t " + p1)
    println("p2 t " + p2)
    println("p3 t " + p3)
    println()
    
    mi.transformSelf(p1)
    mi.transformSelf(p2)
    mi.transformSelf(p3)
    println("p1 tt " + p1)
    println("p2 tt " + p2)
    println("p3 tt " + p3)   
  }
  
  def main(args: Array[String]): Unit = {
    val m1 = new AffineTransform()
    println("m1\t\t" + m1)
    m1.translate(-100,-100)
    println("translate1\t" +m1)
    m1.scale(2,2)
    println("scale1\t\t" +m1)
    m1.translate(100,100)
    println("translate2\t" +m1)
    println()
    
    val p1 = new Point2D(200,300)
    println(p1)
    m1.transformSelf(p1)
    println(p1)
    val mi = m1.copy.inverse
    println("m1=" + m1)
    println("mi=" + mi)
    mi.transformSelf(p1)
    println(p1)
  }
}