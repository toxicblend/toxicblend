package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.vecmath.WeilerAthertonClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.vecmath.ImmutableVec2D
import org.toxicblend.vecmath.MutableVec2D
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.FiniteLine2D
import org.toxicblend.vecmath.Polygon2D


import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class WeilerAthertonTest extends VecMathBaseTest {
  
  val tolerance = 0.0001d
  
  "WeilerAthertonTest-1" should "clip just fine" in {
    
    val subject = toPolygon2D(Seq((10,10),(10,0),(0,0),(0,10)))
    val clip = toPolygon2D(Seq((100,100),(100,0),(0,0),(0,100)))
    //println("subject=" + subject.vertices.mkString(","))
    //println("clip=" + clip.vertices.mkString(","))
    
    val clipped = {
      val rv = WeilerAthertonClipper.clip(subject, clip)
      rv.size should be (1)
      indexByHeading(rv.head.vertices)
    }
    //println("clipped=" + clipped.mkString(","))
    clipped.size should be (4)
    clipped.get(0) should be (subject.vertices(3))
    clipped.get(1) should be (subject.vertices(0))
    clipped.get(2) should be (subject.vertices(1))
    clipped.get(3) should be (subject.vertices(2))
  }
  
  "WeilerAthertonTest-2" should "clip just fine" in {
    
    val subject = toPolygon2D(Seq((10,10),(10,0),(0,0),(0,10)))
    val clip = toPolygon2D(Seq((100,100),(100,0),(0,0),(0,100)))
    //println("subject=" + subject.vertices.mkString(","))
    //println("clip=" + clip.vertices.mkString(","))
    
    val clipped = {
      val rv = WeilerAthertonClipper.clip(subject, clip)
      rv.size should be (1)
      indexByHeading(rv.head.vertices)
    }
    //println("clipped=" + clipped.mkString(","))
    clipped.size should be (4)
    clipped.get(0) should be (subject.vertices(3))
    clipped.get(1) should be (subject.vertices(0))
    clipped.get(2) should be (subject.vertices(1))
    clipped.get(3) should be (subject.vertices(2))
  }
  
  "WeilerAthertonTest-3" should "clip just fine" in {
   
    val subject = toPolygon2D(Seq((10,10),(10,0),(0,0),(0,10)),x=150,y=50)
    val clip = toPolygon2D(Seq((100,100),(100,0),(0,0),(0,100)))
  
    //println("subject=" + subject.vertices.mkString(","))
    //println("clip=" + clip.vertices.mkString(","))
    
    val rv = WeilerAthertonClipper.clip(subject, clip)
  }
  
  "WeilerAthertonTest-4" should "clip just fine" in {
   
    val subject = toPolygon2D(Seq((0,0),(0,100),(100,50)))
    val clip = toPolygon2D(Seq((50,0),(50,100),(150,50)))
  
    //println("subject=" + subject.vertices.mkString(","))
    //println("clip=" + clip.vertices.mkString(","))
    
    val clipped = {
      val rv = WeilerAthertonClipper.clip(subject, clip)
      rv.size should be (1)
      indexByHeading(rv.head.vertices)
    }
    //println("clipped=" + clipped.mkString(","))
    clipped.size should be (3)
    clipped.get(0) should be (Vec2D(50.0,75.0))
    clipped.get(1) should be (Vec2D(100.0,50.0))
    clipped.get(2) should be (Vec2D(50.0,25.0))
  }
  
  "WeilerAthertonTest-5" should "clip just fine" in {
   
    val subject = toPolygon2D(Seq((10d,10d),(10d,100d),(100d,100d),(100d,10d)).reverse,x=100, y=100)
    val clip = toPolygon2D(Seq((0d,0d),(0d,110d),(80d,110d),(110d,50d),(80d,0d)).reverse,x=100, y=100)
  
    //println("subject=" + subject.vertices.mkString(","))
    //println("clip=" + clip.vertices.mkString(","))
    
    val clipped = {
      val rv = WeilerAthertonClipper.clip(subject, clip)
      rv.size should be (1)
      rv.head.vertices
    }
    //println("clipped=" + clipped.mkString(","))
    clipped.size should be (6)
    clipped.get(0) should be (Vec2D(200.0,133.33333333333331))
    clipped.get(1) should be (Vec2D(200.0,170.0))
    clipped.get(2) should be (Vec2D(185.0,200.0))
    clipped.get(3) should be (Vec2D(110.0,200.0))
    clipped.get(4) should be (Vec2D(110.0,110.0))
    clipped.get(5) should be (Vec2D(186.0,110.0))        
  }

  "WeilerAthertonTest-6" should "clip just fine" in {
   
    val subject = toPolygon2D(Seq((409.5291834214155,347.944198069853),(321.29389361213236,347.944198069853),(321.29389361213236,436.1794878791361),(409.5291834214155,436.1794878791361)),scale=1,x=0d,y=0d)
    val clip=toPolygon2D(Seq((497.7644732306985,436.179487879136),(399.98081054687503,331.55681762695315),(388.41041259765626,351.9091003417969),(370.09583740234376,374.49849243164067),(349.5401611328125,390.1649047851563),(327.0698974609375,400.14534301757817),(303.01148681640626,405.6768310546875),(96.04687414419277,436.179487879136)),scale=1d,x=0d,y=0d)
 
    //println("subject=" + subject.vertices.mkString(","))
    //println("clip=" + clip.vertices.mkString(","))
    
    val clipped = {
      val rv = WeilerAthertonClipper.clip(subject, clip)
      rv.size should be (1)
      rv.head
    }
    //println("clipped=" + clipped.mkString(","))
    clipped.size should be (9)
    clipped.isClockwise should be (true)
    clipped.isSelfIntersecting should be (false)
    clipped.isSimple should be (true)      
  }
}
