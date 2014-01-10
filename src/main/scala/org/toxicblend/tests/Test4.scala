package org.toxicblend.tests

import scala.collection.mutable.ArrayBuffer
//import scala.collection.mutable.IndexedSeq
//import scala.collection.mutable.Buffer
import org.toxicblend.typeconverters.SeqShift
import org.toxicblend.typeconverters.SeqOperations
import org.toxicblend.typeconverters.SeqOperations.isPartOf
import org.toxicblend.typeconverters.SeqOperations.mergeLists
import org.toxicblend.typeconverters.SeqOperations.prepareForInsert

object Test4 {
    
  def test(dst:ArrayBuffer[Int], src:Array[Int], edge:(Int,Int), expected:Array[Int], shift:Int) = {
    val dstCopy = ArrayBuffer(dst : _*)
    val srcCopy = ArrayBuffer(src : _*)
    SeqShift.rotate(dstCopy,shift)
    mergeLists(dstCopy,srcCopy,(0,1))
    SeqShift.rotate(dstCopy,-shift)
    if (!dstCopy.sameElements(expected)){
      println("srcCopy=" + srcCopy.mkString("(",",",")")) 
      println("result=" + dstCopy.mkString("(",",",")")) 
      println("expected=" + expected.mkString("(",",",")")) 
    } else {
      println("ok!")
    }
  }
  
  def main(args: Array[String]): Unit = {
    
    val a1 = ArrayBuffer(0,1,2,3)
    val a2 = Array(5,6,7,8,0,1,4)
    val result = Array(0,8,7,6,5,4,1,2,3)
    val edge = (1,0) 
    
    (0 until a1.size).foreach(x => 
      (0 until a2.size).foreach(y => {
        SeqShift.rotate(a2,-1)
        test(a1,a2,edge,result, x)
      })
    )
  } 
}