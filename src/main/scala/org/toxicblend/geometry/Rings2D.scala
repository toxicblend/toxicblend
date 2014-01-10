package org.toxicblend.geometry

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import toxi.geom.ReadonlyVec2D
import scala.annotation.tailrec

/**
 * A container of 2D vertexes that only has two neighbours
 */
class Rings2D(vertexes:ArrayBuffer[ReadonlyVec2D], edges:ArrayBuffer[ArrayBuffer[Int]]) extends Mesh2D(vertexes,edges) {
  
  val rings:ArrayBuffer[ArrayBuffer[Int]] = {
    val vertNeighbours = new HashMap[Int,Array[Int]];

    @inline
    def assignNeighbours(v1:Int, v2:Int) = {
       if (vertNeighbours contains v1) {
        if (vertNeighbours(v1).size >1) {
          if (! vertNeighbours(v1).contains(v2) ) {
            val v = vertNeighbours(v1)
            if (vertNeighbours(v1).contains(v2))
              assert(false, "A vertex in a ring can only have two neighbours. Tried to add " + v2 + " to the list: " + v.mkString("{",",","}"))
          }
        } else {
          val newArray =  new Array[Int](2)
          val v = vertNeighbours(v1)
          newArray(0) = v(0)
          newArray(1) = v2
          vertNeighbours.put(v1, newArray)
        }
      } else {
        val newArray =  new Array[Int](1)
        newArray(0) = v2
        vertNeighbours.put(v1, newArray)
      }
    }
    
    faces.foreach ( f => {
      assert (f.size==2, "A vertex in a ring must have two neighbours.")
      assignNeighbours(f(0), f(1))
      assignNeighbours(f(1), f(0))
    })
    val rv = new ArrayBuffer[ArrayBuffer[Int]]
    val alreadyVisited = new HashSet[Int]
    //println( "faces:" + faces.map( f => f.mkString("{",",","}")).mkString("(",",",")") )
    //println( "vertex neighbours:" + vertNeighbours.map( v => v._1.toString + ":" + v._2.mkString("{",",","}")).mkString("(",",",")") )
        
    @tailrec
    def assignRing(vertex:Int, ring:ArrayBuffer[Int]):ArrayBuffer[Int] = {
      alreadyVisited += vertex
      if (! (vertNeighbours contains vertex)) {
        ring
      } else {
        val nextVertex = vertNeighbours(vertex).filter(v => !( alreadyVisited contains v))
        if ( nextVertex.length == 0 ) {
          ring
        } else {
          ring.append(nextVertex(0))
          assignRing(nextVertex(0),ring)
        }
      }
    }
    
    (0 until vertexes.size).foreach(nextVertex => {
      if ( ! (alreadyVisited contains nextVertex) ) {
        val ring = new ArrayBuffer[Int] += nextVertex
        assignRing(nextVertex,ring)
        if (ring.size > 1) { // ignore rings with zero or just one vertex in it
          rv += ring
        } else {
          System.err.println("Rings2D: Some of the vertexes is not part of any ring. Vertex id:" + nextVertex)
        }
      }
    })
    println("rings:" + rv.size + " individual sizes:" + rv.map( r => r.size ).mkString(",") )
    rv
  }
  
  override
  def toString = "vertexes:" + vertexes.mkString("(",",",")") + " rings: " + rings.map(r => r.mkString("{",",","}")).mkString("(",",",")")
    
}

object Rings2D {
  /**
   * Deep copy ?
   */
  def apply(oldRings2D:Rings2D) = {
    //val newVertexes = new ArrayBuffer[ReadonlyVec2D](oldRings2D.vertexes.size)
    //val newFaces = new ArrayBuffer[ArrayBuffer[Int]](oldRings2D.faces.size)
    //oldRings2D.vertexes.foreach(v => newVertexes += v.copy )
    //oldRings2D.faces.foreach( f => newFaces += f.clone )
    new Rings2D(oldRings2D.vertexes.map(v => v), oldRings2D.faces.map( f => f.map( v => v)))
  }
  
  def apply( vertexes:ArrayBuffer[ReadonlyVec2D], faces:ArrayBuffer[ArrayBuffer[Int]]) = {
    new Rings2D(vertexes, faces)
  } 
}