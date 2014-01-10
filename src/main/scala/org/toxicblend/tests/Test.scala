package org.toxicblend.tests
import scala.collection.mutable.ListBuffer
import org.toxicblend.operations.boostmedianaxis.IntersectionVec3D

object Test {
  
  
  def main1(args: Array[String]): Unit = {
	  val v0 = new IntersectionVec3D(1.1f,10,1)
	  val v1 = new IntersectionVec3D(-1,-10,-1)
	  val v2 = new IntersectionVec3D(-2,10,-.1f)

	  println("v0=" + v0.toString)
 	  println("v1=" + v1.toString)

	  println(v0.intersectsXYPlane(v1,0f).toString)
	  println(v1.intersectsXYPlane(v0,0f).toString)
	  
	  println()
	  
	  println(v2.intersectsXYPlane(v1,0f).toString)
	  println(v1.intersectsXYPlane(v2,0f).toString)
	  
	  println()
	  println(v2.intersectsXYPlane(v0,0f).toString)
	  println(v0.intersectsXYPlane(v2,0f).toString)
	  
	  println(v0.intersectoPoint(v1,0f))
	  println(v1.intersectoPoint(v0,0f))
  }
  
  def splitByPlane(in:Array[IntersectionVec3D], zLimit:Float) : List[List[IntersectionVec3D]] = {
    val rv = new ListBuffer[List[IntersectionVec3D]]
    var subSegment = new ListBuffer[IntersectionVec3D]
    in.iterator.sliding(2,1).withPadding(in.last).toArray.foreach(x => {
      if (x(0).intersectsXYPlane(x(1), zLimit)) {
        if (!subSegment.isEmpty) {
          // Something already added to the sub segment
          // append this intersection point and start a new segment
          subSegment += x(0)
          subSegment += x(0).intersectoPoint(x(1), zLimit)
          rv += subSegment.toList
          subSegment.clear
        } else {
          // fresh subsegment, just append to it
          subSegment += x(0).intersectoPoint(x(1), zLimit)  
        }
      } else {
        if (x(0).z <= zLimit && x(1).z <= zLimit) {
           subSegment += x(0)  
        } else {
          assert (subSegment.isEmpty)
        }
      }
    })
    if (!subSegment.isEmpty){
      rv += subSegment.toList
    }
    rv.toList
  }
  
  def main(args: Array[String]): Unit = {
    /*val a = new ListBuffer[String]
    a += "1"
    a += "2"
    val b = a.toList
    a.clear
    a += "3"
    a += "4"
    
    println(b)
    println(a.toList)
    */
    val points1 = Array(new IntersectionVec3D(0,0,10),new IntersectionVec3D(1,2,-1),new IntersectionVec3D(-6,-2,1) )   
    splitByPlane(points1, .2f).foreach(x => println(x.mkString(",")))
    
    val points2 = Array(new IntersectionVec3D(0,0,10),new IntersectionVec3D(1,2,-1),new IntersectionVec3D(10,20,-1),new IntersectionVec3D(-6,-2,1) )   
    splitByPlane(points2, .2f).foreach(x => println(x.mkString(",")))
    
    val points3 = Array(new IntersectionVec3D(1,2,-1) )   
    splitByPlane(points3, .2f).foreach(x => println(x.mkString(",")))
    
    val points5 = Array(new IntersectionVec3D(0,0,10),new IntersectionVec3D(1,2,-1),new IntersectionVec3D(10,20,-1),new IntersectionVec3D(-6,-2,-1), new IntersectionVec3D(-6,-2,1))   
    splitByPlane(points5, .2f).foreach(x => println(x.mkString(",")))
    
    val points4 = Array(new IntersectionVec3D(1,2,1) )   
    splitByPlane(points4, .2f).foreach(x => println(x.mkString(",")))
    
    println("test6")
    val points6 = Array(new IntersectionVec3D(0,0,10),new IntersectionVec3D(1,2,-1),new IntersectionVec3D(10,20,10),new IntersectionVec3D(-6,-2,-1), new IntersectionVec3D(-6,-2,1))   
    splitByPlane(points6, .2f).foreach(x => println(x.mkString(",")))
    
    println("test7")
    val points7 = Array(new IntersectionVec3D(0,0,10),new IntersectionVec3D(0,0,.1f),new IntersectionVec3D(0,0,10),new IntersectionVec3D(0,0,-10), new IntersectionVec3D(0,0,10))   
    splitByPlane(points7, .2f).foreach(x => println(x.mkString(",")))
    
    println("end")
  }
}