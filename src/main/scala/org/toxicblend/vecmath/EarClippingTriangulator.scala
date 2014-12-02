package org.toxicblend.vecmath

import org.toxicblend.util.CyclicDoubleLinkedArray
import org.toxicblend.util.LinearDoubleLinkedArray

/**
 * Implements ear clipping triangulation
 * Class is not thread safe, but as long as each thread has it's own instance it will be safe to use
 */
class EarClippingTriangulator {
  var input:IndexedSeq[Vec2D] = null
  val vertices = new CyclicDoubleLinkedArray
  val earTips = new LinearDoubleLinkedArray
  val reflexVertices = new LinearDoubleLinkedArray
  //val convexVertices = new LinearDoubleLinkedArray
  val rv = new collection.mutable.ArrayBuffer[Array[Int]]
  
  def isEarTip(i:Int, reflexVerticesHead:Int):Boolean = {
    if (reflexVertices.contains(i)) return false
    var r = reflexVerticesHead
    val ip = vertices.prev(i)
    val in = vertices.next(i)
    val p0 = input(ip)
    val p1 = input(i)
    val p2 = input(in)
    while (r >= 0) {
      if (r!=ip && r!=i && r!= in && Polygon2D.pointInTriangle(input(r), p0, p1, p2)){
        //println("p=" + input(r) + " is inside " + p0 + "->" + p1 + "->" + p2 )
        return false
      } //else println("p=" + input(r) + " is NOT inside " + p0 + "->" + p1 + "->" + p2 )
      r = reflexVertices.next(r)
    }
    true
  }
  
  
  def testAngleOfPoint(p:Int) {
    if (Vec2D.ccw(input(vertices.prev(p)), input(p), input(vertices.next(p))) < 0) {
      //println("" + p + " is convex")
      reflexVertices.safeDrop(p)
      //convexVertices.safeAdd(p)
    } else {
      //println("" + p + " is reflex")
      reflexVertices.safeAdd(p)
      //convexVertices.safeDrop(p)
    }
  }
  
  def testEarTip(p:Int, reflexVerticesHead:Int){
    if (!isEarTip(p, reflexVerticesHead)){
      earTips.safeDrop(p)
    } else {
      earTips.safeAdd(p)
    }
  }
  
  private def triangulate:Boolean = {
    var i = earTips.getOne
    while (i >= 0) {
      earTips.drop(i)
      val vp = vertices.prev(i)
      val vn = vertices.next(i)
      if (vp == vn) {
        //val v = vertices.toIndexedSeq
        //if (v.size > 0) println("early return: " + v.mkString(","))
        return true
      } else {
        rv.append(Array(vp, i, vn))
        //println("added triangle: " + rv.last.mkString(","))
        vertices.drop(i)
        //reflexVertices.safeDrop(i)
        //convexVertices.safeDrop(i)
        //println("removed " + i + " from further processing")
        
        // a convex vertex can never become reflex, so there is no need to test those
        if (reflexVertices.contains(vp)) testAngleOfPoint(vp)
        if (reflexVertices.contains(vn)) testAngleOfPoint(vn)
        val rhead = reflexVertices.head
        testEarTip(vp, rhead)
        testEarTip(vn, rhead)
        i = earTips.getOne
      }
    }
    !vertices.isEmpty
  }
  
  def triangulatePolygon(input:IndexedSeq[Vec2D]):IndexedSeq[Array[Int]] = {
    
    rv.clear
    if (input.size == 3) {
      rv.append(Array(0,1,2))
      return rv
    }
    
    this.input = input
    
    val size = input.size;
    vertices.setup(size)
    earTips.setup(size)
    reflexVertices.setup(size, empty=true)
    //convexVertices.setup(size, empty=true)
    
    //println("reflexVertices =" + reflexVertices.indices.map(i=>"p="+i.prev +",n="+i.next).mkString(","))
    //println("convexVertices =" + convexVertices.indices.map(i=>"p="+i.prev +",n="+i.next).mkString(","))
    
    for(i<-0 until size) 
      testAngleOfPoint(i)
      
    //println("reflexVertices =" + reflexVertices.indices.map(i=>"p="+i.prev +",n="+i.next).mkString(","))
    //println("convexVertices =" + convexVertices.indices.map(i=>"p="+i.prev +",n="+i.next).mkString(","))
    
    var i = earTips.head
    val rhead = reflexVertices.head 
    while (i >= 0) {
      val ii = earTips.next(i)
      testEarTip(i, rhead)
      i = ii
    }
    
    if (false){
      println("initial setup:")
      println("vertices       =" + vertices.toIndexedSeq.mkString(","))
      println("earTips        =" + earTips.toIndexedSeq.mkString(","))
      println("reflexVertices =" + reflexVertices.toIndexedSeq.mkString(","))
      //println("convexVertices =" + convexVertices.toIndexedSeq.mkString(","))
      println
    }
        
    if (triangulate){
      println("detected triangulation trouble:")
      println("vertices       =" + input.mkString(","))
      println("vertex indices =" + vertices.toIndexedSeq.mkString(","))
      println("earTips        =" + earTips.toIndexedSeq.mkString(","))
      println("reflexVertices =" + reflexVertices.toIndexedSeq.mkString(","))
      println("rv             =" + rv.map(t=>t.mkString("{",",","}")).mkString(","))
    }
    rv
  }
}

object EarClippingTriangulator extends App {
  val ec = new EarClippingTriangulator
  val p = Array((250.0,250.0),(400.0,100.0),(100.0,100.0),(100.0,400.0),(280.0,300.0)).map(v=>Vec2D(v._1, v._2))
  val t = ec.triangulatePolygon(p)
  println("Result " + t.map(p=>p.mkString("(",",",")")).mkString(","))
}