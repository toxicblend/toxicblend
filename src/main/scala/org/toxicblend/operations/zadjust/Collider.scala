package org.toxicblend.operations.zadjust

import org.toxicblend.typeconverters.ByteBufferMeshConverter
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec3D

class Collider(val models:IndexedSeq[ByteBufferMeshConverter], val sampleStep:Double, val epsilon:Double) {
  
  val collisionWrapper = new CollisionWrapper(models)
  
  def doCollisionTests(segment:IndexedSeq[Point2d]) : IndexedSeq[Vec3D]= {
    val rv = new ArrayBuffer[Vec3D]
    segment.sliding(2).foreach(segment => {
      val rseg = (collisionWrapper.collisionTestSegment(segment(0), segment(1)))
      rv ++= rseg
    })
    rv
  }
  
  /**
   * iterate over each vertex pair in segments, find the corresponding vertices in levels
   * and add the two
   */
  def adjustZLevel(segments:IndexedSeq[Point2d], levels:IndexedSeq[Vec3D]):IndexedSeq[Vec3D] = {
    new ArrayBuffer[Vec3D]
  }
   
  def cleanup = {
    collisionWrapper.collisionWorld.destroy
  }
}