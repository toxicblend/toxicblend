package org.toxicblend.tests

import org.toxicblend.vecmath.Polygon2D
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.CyclicTree
import org.toxicblend.vecmath.Payload
import org.scalatest._

abstract class VecMathBaseTest extends FlatSpec with Matchers { 
  
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
}