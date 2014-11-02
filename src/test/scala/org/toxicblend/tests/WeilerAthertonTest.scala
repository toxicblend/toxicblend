package org.toxicblend.tests

import org.scalatest._
import org.toxicblend.operations.meshgenerator.vecmath.WeilerAthertonClipper
import org.toxicblend.ToxicblendException
import org.toxicblend.operations.meshgenerator.vecmath.ImmutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.MutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.Vec2D
import org.toxicblend.operations.meshgenerator.vecmath.FiniteLine2D
import org.toxicblend.operations.meshgenerator.vecmath.Polygon2D

import org.toxicblend.attic.{CyclicTree,Payload}
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class WeilerAthertonTest extends FlatSpec with Matchers {
  
  val tolerance = 0.0001d
  def toPolygon2D(seq:Seq[(Int,Int)]):Polygon2D = new Polygon2D(seq.toIndexedSeq.map(p=>Vec2D(p._1,p._2)))
  
  def indexByHeading(seq:IndexedSeq[Vec2D] ) : IndexedSeq[Vec2D] = {
    val angleSequence = seq.map(v => new Payload(v.heading, 0, v) )
    //println(a.map(p=>p.pos).mkString(","))
    //println(a.map(p=>p.angle*180d/math.Pi).mkString(","))
    val rv = CyclicTree.inOrder(angleSequence)._1.map( p => p.pos )
    //println(rv.map(v=>"" + v + "@" + v.heading*180d/math.Pi).mkString(","))
    rv
  }
  
  "WeilerAthertonTest-1" should "clip just fine" in {
    
    val subject = toPolygon2D(Seq((10,10),(10,0),(0,0),(0,10)).map(p=>(p._1-0,p._2-0)))
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
    
    val subject = toPolygon2D(Seq((10,10),(10,0),(0,0),(0,10)).map(p=>(p._1+90,p._2-0)))
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
   
    val subject = toPolygon2D(Seq((10,10),(10,0),(0,0),(0,10)).map(p=>(p._1+150,p._2+50)))
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
}