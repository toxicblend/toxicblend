package org.toxicblend.tests

import scala.collection.mutable.ArrayBuffer
import org.toxicblend.typeconverters.SeqShift
import org.toxicblend.typeconverters.SeqOperations
import org.toxicblend.typeconverters.SeqOperations._

object Test3 {
    
  /*
  def fuseEdges(currentFace:ArrayBuffer[Int], faceToMerge:Seq[Int], edge:(Int,Int)) {
    
    var shiftedFaceToMerge = new Array[Int](faceToMerge.size)
    faceToMerge.copyToArray(shiftedFaceToMerge)
    var shiftedEdge = edge

    var (found,index) = isPartOf(currentFace, shiftedEdge)
    if (found){
      if (index < 0) {
        shiftedEdge = (shiftedEdge._2,shiftedEdge._1)
        val (found2,index2) = isPartOf(currentFace, shiftedEdge)
        index = toIndex(currentFace.size,-index-1) 
        if (index != index2) {
          System.err.println("index2 = " + index2 + " but index=" + index)
          index = index2
        }
      }
    } else {
      System.err.println("error: edge not found in currentFace")
    }
    if (index!=currentFace.size-1)
      SeqShift.rotate(currentFace,-index-1)
    
    var (foundToMerge,indexToMerge) = isPartOf(shiftedFaceToMerge, shiftedEdge)
    if (foundToMerge) {
      if (indexToMerge < 0) {
        SeqShift.reverseInPlace(shiftedFaceToMerge)
      }
    } else {
      System.err.println("error: edge not found in faceToMerge")
    }
    println("indexToMerge = " + indexToMerge + " but index=" + index)

    println("FuseEdges " + currentFace.mkString("{",",","}") + " index=" + index)

    / *
    // shiftedEdge
    if (currentFace.sliding(2).contains(shiftedEdge)) {
      // The direction of shiftedEdge is correct
    } else if (currentFace(0)==shiftedEdge(0) && currentFace(1)==shiftedEdge(1)) {
      // The direction of shiftedEdge is correct
    } else {
      SeqShift.rotate(shiftedEdge,1) // reverse order of shiftedEdge
    }
    * /
    println("FuseEdges " + currentFace.mkString("{",",","}") + " with " + faceToMerge.mkString("{",",","}") + " at " + shiftedEdge)
  }
  */
  
  def test1 = {
    val a1 = SeqOperations.reverseInPlace(ArrayBuffer(0,1,2,3,4))
    val a2 = SeqOperations.reverseInPlace(ArrayBuffer(2,5,6,3))
    //val r = fuseEdges(a1, a2, (2,3))
    var edge = (0,1)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))
    edge = (1,2)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))
    edge = (3,4)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))
    edge = (4,5)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))
    edge = (4,3)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))
    edge = (3,2)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))
    edge = (2,1)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))
    edge = (1,0)
    println("a1=" + a1.mkString("{",",","}") + " edge={" + edge._1 + "," +  edge._2 +"} rv=" + isPartOf(a1,edge))  
  }
  
  def main(args: Array[String]): Unit = {
     var a1 = ArrayBuffer(0,1,2,3)
     SeqOperations.reverseInPlace(a1)
     println("a1=" + a1.mkString("{",",","}"))
     a1 = ArrayBuffer(0,1,2,3,4)
     SeqOperations.reverseInPlace(a1)
     println("a1=" + a1.mkString("{",",","}"))

     var a2 = ArrayBuffer(0,1,5,6,7)
     var edge = (0,1)
     println("a2=" + a2.mkString("{",",","}"))
     mergeLists(a1,a2,edge)
     println("a1=>" + a1.mkString("(",",",")")) 
     a1 = ArrayBuffer(4,0,1,2,3)
     a2 = ArrayBuffer(0,1,5,6,7)
     mergeLists(a1,a2,edge)
     
     a1 = ArrayBuffer(2,3,4,0,1)
     a2 = ArrayBuffer(0,1,5,6,7)
     mergeLists(a1,a2,edge)
     
     a1 = ArrayBuffer(1,2,3,4,0)
     a2 = ArrayBuffer(0,1,5,6,7)
     mergeLists(a1,a2,edge)
  }
}