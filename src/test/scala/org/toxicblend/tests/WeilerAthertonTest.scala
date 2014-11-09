package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.vecmath.WeilerAthertonClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.vecmath.ImmutableVec2D
import org.toxicblend.vecmath.MutableVec2D
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.FiniteLine2D
import org.toxicblend.vecmath.Polygon2D

import org.toxicblend.vecmath.CyclicTree
import org.toxicblend.vecmath.Payload
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class WeilerAthertonTest extends FlatSpec with Matchers {
  
  val tolerance = 0.0001d
  def toPolygon2D(seq:Seq[(Double,Double)], scale:Double=1d, x:Double=0d, y:Double=0d):Polygon2D = {
    Polygon2D(seq.toIndexedSeq.map(p=>Vec2D(p._1*scale+x,p._2*scale+y)))
  }
  
  def indexByHeading(seq:IndexedSeq[Vec2D] ) : IndexedSeq[Vec2D] = {
    val angleSequence = seq.map(v => new Payload(v.heading, 0, v) )
    //println(a.map(p=>p.pos).mkString(","))
    //println(a.map(p=>p.angle*180d/math.Pi).mkString(","))
    val rv = CyclicTree.inOrder(angleSequence)._1.map( p => p.pos )
    //println(rv.map(v=>"" + v + "@" + v.heading*180d/math.Pi).mkString(","))
    rv
  }
  
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
  
}