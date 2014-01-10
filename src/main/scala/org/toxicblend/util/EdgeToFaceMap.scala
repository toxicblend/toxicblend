package org.toxicblend.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

/**
 * A hash container keyed by edge. Pointing to all faces that contain that edge
 */
class EdgeToFaceMap {
  /**
   * (point1,point2) => Set[faceId...]
   */
  val edge2facesMap = new HashMap[(Int,Int),HashSet[Int]]
  
  /**
   * faceId => Set[ (point1,point2)...]
   */
  val face2EdgesMap = new HashMap[Int,HashSet[(Int,Int)]]
  
  @inline
  def add(key:Seq[Int], faceId:Int):EdgeToFaceMap = {
     add((key(0),key(1)),faceId)
  }
  
  @inline
  def sortEdge(edge:(Int,Int)) : (Int,Int)= {
    if (edge._1 < edge._2 ) 
      edge 
    else 
      (edge._2, edge._1)
  }
  
  def add(key:(Int,Int), faceId:Int):EdgeToFaceMap = {
    val realKey = sortEdge(key)
    if (edge2facesMap contains realKey) {
      edge2facesMap(realKey) += faceId
    } else {
      edge2facesMap(realKey) = new HashSet[Int]() += faceId
    }
    if (face2EdgesMap contains faceId) {
      face2EdgesMap(faceId).add(realKey)
    } else {
      face2EdgesMap(faceId) = new HashSet[(Int,Int)] += realKey
    }
    EdgeToFaceMap.this
  }
  
  def remove(faceId:Int) = {
    face2EdgesMap(faceId).foreach( edge => {
      edge2facesMap(edge).remove(faceId) 
    })
    face2EdgesMap.remove(faceId)
  }
  
  def apply(key:(Int,Int)) = {
    val realKey = if (key._1 < key._2 ) key else (key._2, key._1)
    edge2facesMap(realKey)
  }
  
  def contains(key:(Int,Int)):Boolean = {
    val realKey = if (key._1 < key._2 ) key else (key._2, key._1)
    edge2facesMap contains realKey
  }
}