package org.toxicblend.vecmath

import org.toxicblend.util.CyclicDoubleLinkedArray
import org.toxicblend.util.LinearDoubleLinkedArray
import org.toxicblend.util.SortedLinearDoubleLinkedArray
import org.toxicblend.util.NumberUtils.r2d

final class Triangles private () extends Traversable[Array[Int]]{
  private var numberOfElements = 0
  private var data:Array[Array[Int]] = null
  
  def this(initialSize:Int) = {
    this()   
    data = new Array[Array[Int]](initialSize)
    for (i<-0 until initialSize) data(i) = Array(0,0,0)
    numberOfElements = 0
  }
  
  def append(p0:Int,p1:Int,p2:Int) = {
    if (numberOfElements+1 >= data.size) {
      ensureCapacity((numberOfElements+1)*2)
    }
    val e = data(numberOfElements)
    e(0) = p0
    e(1) = p1
    e(2) = p2
    numberOfElements += 1
  }
  
  def minumumAngle(vertices:IndexedSeq[Vec2D], p0:Int, p1:Int, p2:Int): (Double,Int) = {
    val v0 = vertices(p0)
    val v1 = vertices(p1)
    val v2 = vertices(p2)
    val a0 = -Vec2D.normalizedDotSquaredWithSign(v0, v1, v2)
    val a1 = -Vec2D.normalizedDotSquaredWithSign(v1, v2, v0)
    val a2 = -Vec2D.normalizedDotSquaredWithSign(v2, v0, v1)
    val rv = if (a0 < a1) {
      if (a0 < a2) (-a0,p0)
      else (-a2,p2)
    } else {
      // a0 >= a1
      if (a1 < a2) (-a1,p1)
      else (-a2,p2)
    }   
    assert(rv._1 == -math.min(math.min(a0, a1),a2))
    rv
  }
  
  @inline def isOuterEdge(maxVertices:Int, a:Int, b:Int):Boolean = {
    if ( b == a+1 || b == a-1) return true
    if ( a == maxVertices && b==0) return true
    if ( b == maxVertices && a==0) return true
    false
  }
  
  def rotate(triangle:Array[Int], forwardOneStep:Boolean):Unit = {
     if (forwardOneStep) {
      val tmp = triangle(2)
      triangle(2) = triangle(1)
      triangle(1) = triangle(0)
      triangle(0) = tmp
    } else {
      // backward one step
      val tmp = triangle(0)
      triangle(0) = triangle(1)
      triangle(1) = triangle(2)
      triangle(2) = tmp
    }
  }
  
  def searchAndRotate(triangle:Array[Int], a:Int, b:Int) :Boolean = {
    if (triangle(0) == a && triangle(1) == b) return true
    if (triangle(1) == a && triangle(2) == b) {
      rotate(triangle, false)
      assert(triangle(0)==a)
      assert(triangle(1)==b)
      return true
    }
    if (triangle(2) == a && triangle(0) == b) {
      rotate(triangle, true)
      assert(triangle(0)==a)
      assert(triangle(1)==b)
      return true
    }
    false
  }
  
  def edgeSwap(a:Int, b:Int, vertices:IndexedSeq[Vec2D], minAngle1:Double) : Boolean = {
    val t1 = data(numberOfElements-1)
    searchAndRotate(t1,a,b)
    for (i<- 0 until numberOfElements -1) {
      if (!searchAndRotate(data(i),b,a)) {
        //println("found nothing to edge swap with: " + t1.mkString("(",",",")") + " a=" + a + " b=" + b + " data(i)=" + data(i).mkString("(",",",")"))
      } else {
        val t2 = data(i)
        val t12 = t1(2) 
        val t20 = t2(0)
        val t21 = t2(1)
        val t22 = t2(2)
        val t11 = t1(1)
        val t10 = t1(0)
        assert(t11 == t20)
        assert(t21 == t10)
        
        if ( Vec2D.ccw(vertices(t22), vertices(t11), vertices(t12)) < 0 &&
             Vec2D.ccw(vertices(t12), vertices(t10), vertices(t22)) < 0 ) {
          //println("found swappable candidate : " + t2.mkString("(",",",")") + " to: " + t1.mkString("(",",",")"))
          val minAngleÚnaltered = math.min(minAngle1, minumumAngle(vertices,t20,t21,t22)._1 )
          val minAngleAltered = math.min( minumumAngle(vertices,t12,t22,t11)._1, 
                                          minumumAngle(vertices,t10,t22,t12)._1 )
          //println("minAngleÚnaltered=" + minAngleÚnaltered)
          //println("minAngleAltered=" + minAngleAltered)
            
          if (minAngleÚnaltered >= minAngleAltered) {
            // do nothing 
            //println("keep as is")  
          } else {
            // do the edge swap
            //println("do the edge swap") 
            
            t1(0) = t12
            t1(1) = t22
            t1(2) = t11
            
            t2(0) = t10
            t2(1) = t22
            t2(2) = t12
            //println("swapped to t1=" + t1.mkString("(",",",")") + " t2:=" + t2.mkString("(",",",")"))
            return true
          }
        }
      }
    }
    false
  }
  
  def appendAndOptimize(vertices:IndexedSeq[Vec2D], p0:Int, p1:Int, p2:Int) = {
    val maxVertices = vertices.size - 1
    val (minAngle, minPos) = minumumAngle(vertices,p0,p1,p2)
    val minAngle2 = if (minAngle < 0) math.acos(-math.sqrt(-minAngle)) else math.acos(math.sqrt(minAngle))
    //println("minimum angle of " + p0 + "," + p1 + "," + p2 + " is " + r2d(minAngle2))
    append(p0,p1,p2)
    if (!isOuterEdge(maxVertices, p1, p2) && edgeSwap(p1, p2, vertices, minAngle)) {}
    else if (!isOuterEdge(maxVertices, p0, p1) && edgeSwap(p0, p1, vertices, minAngle)) {} 
    else if (!isOuterEdge(maxVertices, p2, p0) && edgeSwap(p2, p0, vertices, minAngle)) {}
  }
  
  override def size = numberOfElements
  @inline def apply(i:Int) = data(i)
  
  def foreach[U](f: Array[Int] => U) = for (i<- 0 until numberOfElements) f(data(i))
    
  def ensureCapacity(newSize:Int) = {
    // resize array and copy old data
    if(newSize>data.size){
      val newData = new Array[Array[Int]](newSize)
      for (i<-0 until data.size) {
        newData(i) = data(i)
        data(i)=null
      }
      for (i<-data.size until newData.size) newData(i) = Array(0,0,0)
      data = newData
    }
  }
  
  def isAllClockwise(vertices:IndexedSeq[Vec2D]) : Boolean = {
    this.foreach(p => if (! Polygon2D.isClockwise(vertices(p(0)), vertices(p(1)), vertices(p(2)))) return false ) 
    false
  }
  
  /**
   * won't deallocate anything, just sets the virtual size to 0 
   */
  def clear = numberOfElements = 0
}

/**
 * Implements ear clipping triangulation.
 * Class is not thread safe, but as long as each thread has it's own instance it will be safe to use.
 * By tradition this class operates in allocation free mode.
 * Every data container is reused. So make sure you use or copy result of the triangulation in between invocations. 
 */
class EarClippingTriangulator(val useQualityTriangulation:Boolean) {
  var input:IndexedSeq[Vec2D] = null
  val vertices = new CyclicDoubleLinkedArray
  val earTips = new SortedLinearDoubleLinkedArray[Double](initialValue=0d, setupAsEmpty=true)
  
  val reflexVertices = new LinearDoubleLinkedArray
  val rv = new Triangles(10)
  
  def isEarTip(i:Int):Boolean = {
    if (reflexVertices.contains(i)) return false
    var r = reflexVertices.head
    val ip = vertices.prev(i)
    val in = vertices.next(i)
    if (ip == in) return false
    val p0 = input(ip)
    val p1 = input(i)
    val p2 = input(in)
    while (r >= 0) {
      if (r!=ip && r!=i && r!= in && Polygon2D.pointInTriangle(input(r), p0, p1, p2)){
        return false
      } 
      r = reflexVertices.next(r)
    }
    true
  }
  
  def testAngleOfPoint(p:Int) = {
    if (Vec2D.ccw(input(vertices.prev(p)), input(p), input(vertices.next(p))) < 0) {
      reflexVertices.safeDrop(p)
    } else {
      reflexVertices.safeAdd(p)
    }
  }
  
  def getAngleOfPoint(p:Int) = -Vec2D.normalizedDotSquaredWithSign(input(p), input(vertices.prev(p)), input(vertices.next(p)))
  
  def testEarTip(p:Int) = {
    if (!isEarTip(p)) earTips.safeDrop(p)
    else {
      if (earTips.contains(p)) earTips.update(p, getAngleOfPoint(p))
      else earTips.add(p, getAngleOfPoint(p))
    }
  }
  
  private def triangulate:Boolean = {
    var i = earTips.head
    while (i >= 0) {
      val eartipAngle = earTips(i)
      earTips.drop(i)
      val vp = vertices.prev(i)
      val vn = vertices.next(i)
      if (vp == vn) {
        return true
      } else {
        if (useQualityTriangulation) rv.appendAndOptimize(input, vp, i, vn)
        else rv.append(vp, i, vn)
        
        vertices.drop(i)
        reflexVertices.safeDrop(i)
        //println("removed " + i + " from further processing")
        
        // a convex vertex can never become reflex, so there is no need to test those
        if (reflexVertices.contains(vp)) testAngleOfPoint(vp)
        if (reflexVertices.contains(vn)) testAngleOfPoint(vn)
        testEarTip(vp)
        testEarTip(vn)
        //println(earTips.toIndexedSeq.map(p=>earTips(p).value).mkString(","))
        i = earTips.head
      }
    }
    {
      i = vertices.getOne
      !(vertices.isEmpty || (vertices.next(vertices.next(i)) == i)) 
    } || !rv.isAllClockwise(input)
  }
  
  def triangulatePolygon(input:IndexedSeq[Vec2D]):Triangles = {
    rv.clear
    rv.ensureCapacity(input.size-2)
    
    if (input.size == 3) {
      rv.append(0,1,2)
      return rv
    }
    
    this.input = input
    
    val size = input.size;
    vertices.setup(size)
    earTips.setup(size, 0, setupAsEmpty=false)
    reflexVertices.setup(size, setupAsEmpty=true)
  
    for(i<-0 until size) 
      testAngleOfPoint(i)
        
    var i = earTips.head
    while (i >= 0) {
      val ii = earTips.next(i)
      testEarTip(i)
      i = ii
    }
    
    if (false){
      println("initial setup:")
      println("vertices       =" + vertices.toIndexedSeq.mkString(","))
      println("earTips        =" + earTips.toIndexedSeq.map(i => "#" + i + "@" + earTips(i).value).mkString(","))
      println("reflexVertices =" + reflexVertices.toIndexedSeq.mkString(","))
      println
    }
        
    if (triangulate){
      println("detected triangulation trouble:")
      println("input             =" + input.mkString(","))
      println("vertex indices    =" + vertices.toIndexedSeq.mkString(","))
      println("earTips           =" + earTips.toIndexedSeq.mkString(","))
      println("reflexVertices    =" + reflexVertices.toIndexedSeq.mkString(","))
      println("rv                =" + rv.map(t=>t.mkString("{",",","}")).mkString(","))
      println("rv.isAllClockwise =" + rv.isAllClockwise(input))
      println
    }
    rv
  }
}
