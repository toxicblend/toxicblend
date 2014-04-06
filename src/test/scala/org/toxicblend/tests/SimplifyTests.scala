package org.toxicblend.tests

import toxi.geom.Vec3D
import org.toxicblend.geometry.QuickLineSegment
import org.toxicblend.geometry.RamerDouglasPeuckerAlgorithm.simplify
import org.scalatest._
import matchers.ShouldMatchers._

/**
 * 
 */
class SimplifyTests extends FlatSpec with Matchers {
  
  "LineSegment-1" should "calculate correct distance" in {    
    val segment = new QuickLineSegment(new Vec3D(0f,1f,0f), new Vec3D(0f,-1f,0f))
    val p = new Vec3D(-1f,0f,0f)
    segment.distanceSquared(p) should be (1f)
  }
  
  "LineSegment-2" should "calculate correct distance" in {    
    val segment = new QuickLineSegment(new Vec3D(0f,0f,0f), new Vec3D(1f,0f,0f))
    val p = new Vec3D(2f,0f,0f)
    val d = segment.distanceSquared(p)
    d should not be (Float.NaN)
    d should be (1f)
  }
  
  "SimplifyTests-1" should "simplify nothing" in {    
    val segment = Array(new Vec3D(0f,-1f,0f), 
                        new Vec3D(-1f,0f,0f),
                        new Vec3D(0f,1f,0f),
                        new Vec3D(1f,0f,0f),
                        new Vec3D(0f,-1f,0f))                                       
    val result = simplify(segment, 0.1f)
    result.size should be (segment.size)
    
    (0 until segment.size).foreach(i => {
      result(i) should be (segment(i))  
    })
  }
  
  "SimplifyTests-2" should "simplify everything" in {    
    val segment = Array(new Vec3D(0f,1f,1f), 
                        new Vec3D(-1f,0f,1f),
                        new Vec3D(0f,1f,1f),
                        new Vec3D(1f,0f,1f),
                        new Vec3D(0f,-1f,1f))                                       
    val result = simplify(segment, 2f)
    result.size should be (2)
    result.head should be (segment.head)
    result.last should be (segment.last)
  }
}

