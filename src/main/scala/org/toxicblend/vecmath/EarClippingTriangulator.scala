package org.toxicblend.vecmath

import org.toxicblend.util.CyclicDoubleLinkedArray
import org.toxicblend.util.LinearDoubleLinkedArray

class EarClippingTriangulator {
  var input:IndexedSeq[Vec2D] = null
  val vertices = new CyclicDoubleLinkedArray
  val earTips = new LinearDoubleLinkedArray
  val reflexVertices = new LinearDoubleLinkedArray
  val convexVertices = new LinearDoubleLinkedArray
  
  def isEarTip(i:Int, reflexVerticesHead:Int):Boolean = {
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
      convexVertices.safeAdd(p)
    } else {
      //println("" + p + " is reflex")
      reflexVertices.safeAdd(p)
      convexVertices.safeDrop(p)
    }  
  }
  
  def testEarTip(p:Int, reflexVerticesHead:Int){
    if (!isEarTip(p, reflexVerticesHead)){
      earTips.safeDrop(p)
    } else {
      earTips.safeAdd(p)
    }
  }
  
  def triangulate(input:IndexedSeq[Vec2D]):IndexedSeq[Array[Int]] = {
    this.input = input
    val rv = new collection.mutable.ArrayBuffer[Array[Int]]
    
    val size = input.size;
    vertices.setup(size)
    earTips.setup(size)
    reflexVertices.setup(size)
    convexVertices.setup(size)
    
    for(i<-0 until size) 
      testAngleOfPoint(i)
    //println("reflexVertices =" + reflexVertices.toIndexedSeq.mkString(","))
    //println("convexVertices =" + convexVertices.toIndexedSeq.mkString(","))
    
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
      println("convexVertices =" + convexVertices.toIndexedSeq.mkString(","))
      println
    }
    
    i = earTips.getOne
    while (i >= 0) {
      earTips.drop(i)
      val vp = vertices.prev(i)
      val vn = vertices.next(i)
      if (vp == vn) {
        i = -1
      } else {
        rv.append(Array(vp, i, vn))
        //println("added triangle: " + rv.last.mkString(","))
        vertices.drop(i)
        if (reflexVertices.contains(i)) reflexVertices.drop(i)
        if (convexVertices.contains(i)) convexVertices.drop(i)
        //println("removed " + i + " from further processing")
        testAngleOfPoint(vp)
        testAngleOfPoint(vn)
        val rhead = reflexVertices.head
        testEarTip(vp, rhead)
        testEarTip(vn, rhead)
        i = earTips.getOne
      }
    }
    
    println("vertices       =" + vertices.toIndexedSeq.mkString(","))
    println("earTips        =" + earTips.toIndexedSeq.mkString(","))
    println("reflexVertices =" + reflexVertices.toIndexedSeq.mkString(","))
    println("convexVertices =" + convexVertices.toIndexedSeq.mkString(","))
    rv
  }
}

object EarClippingTriangulator extends App {
  val ec = new EarClippingTriangulator
  val p = Array(Vec2D(0,0),Vec2D(1,0),Vec2D(1,1), Vec2D(0,1) )
  val t = ec.triangulate(p)
  println("Result " + t.map(p=>p.mkString("(",",",")")).mkString(","))
}