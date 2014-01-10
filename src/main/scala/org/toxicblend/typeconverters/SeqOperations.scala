package org.toxicblend.typeconverters
import scala.collection.mutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer

object SeqOperations {
  
  @inline
  def reverseInPlace[A](a: IndexedSeq[A]) = {
    // even size
    var tmp = a(0)
    (0 until a.size/2).foreach(i => {
      tmp = a(i)
      a(i) = a(a.size-i-1)
      a(a.size-i-1) = tmp
    })
    a
  }
  
  @inline
  def toIndex(size:Int,i:Int):Int = {
    if (i<0) i%size+size 
    else if (i>size-1) i%size 
    else i 
  }
    
  /**
   * returns (true,index) if edge is found inside sequence. index is the position of edge._1
   * returns (true,-index) if edge.reverse() is found inside sequence. index is still the position of edge._1 though
   * else return (false,0)
   */
  def isPartOf(sequence:Seq[Int], edge:(Int,Int)):(Boolean,Int) = {
    if (sequence.length < 2) return (false,0)
    // val toIndex = wraparoudIndex(seq.size)
    (0 until sequence.size).foreach( i => {
      if (sequence(i) == edge._1 ) {
        if (sequence(toIndex(sequence.size,i+1)) == edge._2) {
          return (true,i)
        } else if (sequence(toIndex(sequence.size,i-1)) == edge._2){
          return (true,-i)
        }
      } 
    }) 
    (false,0)
  }
  
  /** 
   * Rotate and/or reverse the list until it starts with edge._2 and ends with edge._1
   * The operations is "in place"
   * Then remove first and last element
   */ 
  def prepareForInsert(array:ArrayBuffer[Int], edge:(Int,Int) ) = {
    var (found,index) = isPartOf(array,edge)
    if (found) {
      if (index>=0) {
        SeqOperations.reverseInPlace(array)
        val (found2,index2) = isPartOf(array,edge)
        val oldIndex=index
        index = index-array.size+1
        if (index!=index2) {
          System.err.println("prepareForInsert: oldIndex=" + oldIndex + " index=" + index + " index2=" + index2 +" array:" +  array.mkString("(",",",")"))
          index = index2
        }
      } 

      SeqShift.rotate(array, index)
      array.remove(array.size-1)
      array.remove(0)
    } else {
      System.err.println("prepareForInsert: edge not found in array:" +  array.mkString("(",",",")"))
    }
    found
  }
  
  /**
   * merge the src list into the dst list at the point of edge  
   */
  def mergeLists(dst:ArrayBuffer[Int], src:IndexedSeq[Int], edge:(Int,Int)) = {
    var (found,index) = isPartOf(dst,edge)
    // make a copy of src because we gonna mess with it
    val srcCopy = ArrayBuffer(src : _*)
    if (found) {
      if (index < 0) {
        // reverse the edge
        val newEdge = (edge._2, edge._1)
        if ( prepareForInsert(srcCopy,newEdge) ){
          val (found2,index2) = isPartOf(dst,newEdge)
          val oldIndex = index
          index = toIndex(dst.size,-index-1)
          if (index2!=index) {
            System.err.println("mergeLists: oldIndex=" + oldIndex + " edge=" + newEdge + " index=" + index + " index2=" + index2 +" array:" +  dst.mkString("(",",",")"))
            index = index2
          }
        } 
      } else { 
        if ( !prepareForInsert(srcCopy,edge) ){
          System.err.println("mergeLists: edge =" + edge + " not found in " + srcCopy.mkString("(",",",")"))
        }
      } 
      dst.insertAll(index+1, srcCopy)
    } else {
      found = false
    }
    found
  }
}