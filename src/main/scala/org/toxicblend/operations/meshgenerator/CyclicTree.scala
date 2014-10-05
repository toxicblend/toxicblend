package org.toxicblend.operations.meshgenerator

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import toxi.geom.ReadonlyVec2D
import toxi.geom.Vec2D
import org.toxicblend.ToxicblendException
//import scala.annotation.tailrec

trait Tree {
 
  def value: Option[Int] = this match {
    case n: Node => Some(n.v)
    case l: Leaf => Some(l.v)
    case Empty => None
  }
  
  /**
   * Why does this method sometimes return Some(Empty), that should be impossible, yet it happends??? 
   */
  def left: Option[Tree] = this match {
    case n: Node => Some(n.l)
    case l: Leaf => None
    case Empty   => None
    case _ => None
  }
  
  /**
   * Why does this method sometimes return Some(Empty), that should be impossible, yet it happends??? 
   */
  def right: Option[Tree] = this match {
    case n: Node => Some(n.r)
    case l: Leaf => None
    case Empty   => None
    case _ => None
  }
  
  def hasValue = true
  
  def treeMinimum = {
    var x = this
    while (x.left.isDefined) {
      x = x.left.get
    }
    x.value.get
  }
  
  def treeMaximum = {
    var x = this
    while (x.right.isDefined) {
      x = x.right.get
    }
    x.value.get
  }

  protected[meshgenerator] def searchWithLimits(seq:IndexedSeq[Payload], searchAngle:Double, lowerLimit:Int, upperLimit:Int) : Option[(Int,Int)] = {
    if ( searchAngle < seq(value.get).angle) {
      val l1 = left
      if (l1.isDefined && l1.get.hasValue)
        l1.get.searchWithLimits(seq, searchAngle, lowerLimit, value.get)
      else
        Some(lowerLimit, value.get)
    } else {
      val r1 = right 
      if (r1.isDefined && r1.get.hasValue)
        r1.get.searchWithLimits(seq, searchAngle, value.get, upperLimit)
      else 
        Some(value.get, upperLimit)
    }      
  }
    
  private def toSequenceRec(seq:IndexedSeq[Payload], b:Buffer[Payload]):Unit = {
    if (left.isDefined) 
      left.get.toSequenceRec(seq, b)
    if (value.isDefined) 
      b.append(seq(value.get))
    if (right.isDefined)
      right.get.toSequenceRec(seq, b)
  }
  
  def toSequence(seq:IndexedSeq[Payload]):Buffer[Payload] = {
    val rv = new ArrayBuffer[Payload]
    toSequenceRec(seq,rv)
    rv
  }
}

case class Node(v: Int, l: Tree, r: Tree) extends Tree
case class Leaf(v: Int) extends Tree
case object Empty extends Tree {
  override def hasValue = false
}

class Payload (val angle:Double, val distance:Double, val pos:ReadonlyVec2D)
object Payload {
  def apply(angle:Double) = new Payload(angle, 0, new Vec2D(0,0))
  def apply(angle:Double, distance:Double, pos:ReadonlyVec2D) = new Payload(angle, distance, pos)
}

class CyclicTree(private val seq:IndexedSeq[Payload], private val tree:Tree){
   
  def searchIntervalWithLimits(searchAngle:Double) : Option[(Payload,Payload)] = {
    if (seq.size <= 0) {
      None
    } else {
      if (searchAngle > seq.last.angle ) {
        Some(seq.last, seq.head)
      } else  if (searchAngle <= seq.head.angle) {
        Some(seq.last, seq.head)
      } else {
        
        val res = tree.searchWithLimits(seq, searchAngle, -1, -1)
        if (res.isDefined) {
          var result = res.get 
          if (result._1 == -1) {
            result = (0,result._2)
          }
          if (result._2 == -1) {
            if (result._1 == seq.size-1) 
              result = (result._1,0) 
            else 
              result = (result._1,seq.size-1)
          }
          Some(seq(result._1), seq(result._2) )
        } else {
          None
        }
      }  
    }
  }
  
  override def toString = {
    tree.toSequence(seq).mkString(",")
  }
}

object CyclicTree {
  
  /**
   * makes sure the smallest element of the sequence is at the head by copying the sequence if needed
   */
  def inOrder(seq:IndexedSeq[Payload]) : IndexedSeq[Payload] = {
    if (seq.size > 0) {
      val smallest = (0 until seq.size).reduceLeft( (x,a) => if (seq(x).angle<seq(a).angle) x else a )
      if (smallest == 0) {
        seq
      } else {
        val rv = new Array[Payload](seq.size)
        var dest = 0
        for (i <- smallest until seq.size) {
          rv(dest) = seq(i)
          dest+=1
        }
        for (i <- 0 until smallest) {
          rv(dest) = seq(i)
          dest+=1
        }
        (0 until rv.size).reduceLeft( (x,a) => if (rv(x).angle>rv(a).angle) throw new ToxicblendException("Non-convex shape?") else a)
        rv
      }
    } else seq
  }
  
  def apply(aSeq:IndexedSeq[Payload]):CyclicTree  = {
    val seq = inOrder(aSeq)
    val rootNode = if (seq.size > 0) {
      val centerPos = (seq.size-1)/2
      val leftPos = 0
      val rightPos = seq.size-1
      this.apply(seq, leftPos, centerPos, rightPos)
    } else {
      Empty
    }
    new CyclicTree(seq, rootNode)
  }
  
  private def apply(values:IndexedSeq[Payload], leftPos:Int, centerPos:Int, rightPos:Int) : Tree = {
    //println("leftPos=" + leftPos + " centerPos=" + centerPos + " rightPos=" + rightPos)
    if (centerPos < 0 || leftPos < 0 || rightPos < 0){
      Empty
    }
    if (centerPos - leftPos == 0 && rightPos - centerPos == 0) {
      Leaf(centerPos)
    }
    else 
    {
      val left = centerPos - leftPos match {
        case 0 => Empty
        case 1 => Leaf(leftPos)
        case x:Int => this.apply(values, leftPos, leftPos + (centerPos-leftPos)/2,  centerPos-1)
      }
      val right = rightPos - centerPos match {
        case 0 => Empty
        case 1 => Leaf(rightPos)
        case x:Int => this.apply(values, centerPos+1, centerPos + (1+rightPos-centerPos)/2, rightPos)
      }
     new Node(centerPos, left, right)
    }
  }
}
