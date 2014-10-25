package org.toxicblend.attic

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import org.toxicblend.operations.meshgenerator.vecmath.ImmutableVec2D
import org.toxicblend.operations.meshgenerator.vecmath.Vec2D
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
  

  protected[attic] def 
  searchWithLimits(seq:IndexedSeq[Payload], searchAngle:Double, lowerLimit:Int, upperLimit:Int) : Option[(Int,Int)] = {
    if (searchAngle < seq(value.get).angle) {
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

class Payload (val angle:Double, val distance:Double, val pos:Vec2D)
object Payload {
  def apply(angle:Double) = new Payload(angle, 0, new ImmutableVec2D(0,0))
  def apply(angle:Double, distance:Double, pos:Vec2D) = new Payload(angle, distance, pos)
}

class CyclicTree(val seq:IndexedSeq[Payload], private val tree:Tree, val clockwise:Boolean){
  
  @inline protected final def isFlipside(searchAngle:Double) =
    (clockwise && (searchAngle < seq.last.angle || searchAngle >= seq.head.angle) ||
     !clockwise && (searchAngle > seq.last.angle || searchAngle <= seq.head.angle)) 
  
  def searchIntervalWithLimits(searchAngle:Double) : Option[(Payload,Payload)] = {
    if (seq.size <= 0) None
    else {
      val res = if (isFlipside(searchAngle)) Some(seq.size-1, 0)
                else tree.searchWithLimits(seq, searchAngle, -1, -1) 
      if (res.isDefined) {
        var result = res.get 
        if (result._1 == -1) result = (0,result._2)
        if (result._2 == -1) {
          if (result._1 == seq.size-1) result = (result._1,0) 
          else result = (result._1,seq.size-1)
        }
        Some(seq(result._1), seq(result._2) )
      } else None
    }
  }
  
  override def toString = {
    tree.toSequence(seq).mkString(",")
  }
  
  def toSequence = tree.toSequence(seq)
}

object CyclicTree {
  /**
   * http://en.wikipedia.org/wiki/Shoelace_formula
   * Returns true if the polygon is clockwise
   */
  def shoelace(seq:IndexedSeq[Payload]):Boolean = {
    //var sum=0f
    //for (i <- 0 until seq.size-1) sum += seq(i).pos.x*(seq(i+1).pos.y-seq(i+1).pos.x*seq(i).pos.x)
    //sum += seq.last.pos.x*seq.head.pos.y - seq.head.pos.x*seq.last.pos.y
    var sum2 = (0 until seq.size-1).foldLeft(0d) ((s,i)=>s+seq(i).pos.x*(seq(i+1).pos.y-seq(i+1).pos.x*seq(i).pos.x)) +
               seq.last.pos.x*seq.head.pos.y - seq.head.pos.x*seq.last.pos.y
    //if (sum != sum2) println("sum2differ " + sum + " " + sum2)
    sum2 < 0
  }
  
  /**
   * makes sure the elements of the sequence are in order. Cyclic ordering is not altered
   * @param clockwise: if the ordering is known it can be specified here, if not - use None and it will be calculated
   */
  def inOrder(seq:IndexedSeq[Payload], clockwise:Option[Boolean]=None) : (IndexedSeq[Payload],Boolean) = {
    if (seq.size > 0) {
      val isClockwise = if (clockwise.isDefined) clockwise.get else shoelace(seq)
      //println("clockwise=" + isClockwise )
      //println("original=" + seq.map(v=> "@" + v.angle).mkString(","))
          
      if (isClockwise) {
        // clockwise: angles should decrease as we go down the sequence
        val largestAtPos = (0 until seq.size).reduceLeft( (x,a) => if (seq(x).angle>seq(a).angle) x else a )
        if (largestAtPos == 0) {
          (seq,isClockwise)
        } else {
          val rv = new Array[Payload](seq.size)
          var dest = 0
          for (i <- largestAtPos until seq.size) {
            rv(dest) = seq(i)
            dest+=1
          }
          for (i <- 0 until largestAtPos) {
            rv(dest) = seq(i)
            dest+=1
          }
          //println("clockwise=" + isClockwise + " largestAtPos=" + largestAtPos)
          //println("converted=" + rv.map(v=>"" + "@" + v.angle*180d/math.Pi).mkString(","))
          (0 until rv.size).reduceLeft( (a,x) => if (rv(x).angle>rv(a).angle) throw new ToxicblendException("Non-convex shape?") else x)
          (rv,isClockwise)
        }
      } else {
        // Anti-clockwise: angles should increase as we go down the sequence
        val smallestAtPos = (0 until seq.size).reduceLeft( (x,a) => if (seq(x).angle<seq(a).angle) x else a )
        if (smallestAtPos == 0) {
          (seq,isClockwise)
        } else {
          val rv = new Array[Payload](seq.size)
          var dest = 0
          for (i <- smallestAtPos until seq.size) {
            rv(dest) = seq(i)
            dest+=1
          }
          for (i <- 0 until smallestAtPos) {
            rv(dest) = seq(i)
            dest+=1
          }
          //println("clockwise=" + isClockwise + " smallestAtPos=" + smallestAtPos)
          //println("converted=" + rv.map(v=>"" + "@" + v.angle).mkString(","))
          (0 until rv.size).reduceLeft( (a,x) => if (rv(x).angle<rv(a).angle) throw new ToxicblendException("Non-convex shape?") else x)
          (rv,isClockwise)
        }
      }
    } else (seq, false)
  }
  
  def apply(aSeq:IndexedSeq[Payload], inClockwise:Option[Boolean]=None):CyclicTree  = {
    val (seq, clockwise) = inOrder(aSeq, inClockwise)
    //println(seq.map(p=>p.angle))
    val rootNode = if (seq.size > 0) {
      val centerPos = (seq.size-1)/2
      val leftPos = 0
      val rightPos = seq.size-1
      apply(seq, leftPos, centerPos, rightPos, clockwise)
    } else {
      Empty
    }
    new CyclicTree(seq, rootNode, clockwise)
  }
  
  private def apply(values:IndexedSeq[Payload], leftPos:Int, centerPos:Int, rightPos:Int, clockwise:Boolean) : Tree = {
    if (centerPos < 0 || leftPos < 0 || rightPos < 0){
      Empty
    } else if (centerPos - leftPos == 0 && rightPos - centerPos == 0) {
      Leaf(centerPos)
    } else {
      val left = centerPos - leftPos match {
        case 0 => Empty
        case 1 => Leaf(leftPos)
        case x:Int => apply(values, leftPos, leftPos + (centerPos-leftPos)/2,  centerPos-1, clockwise)
      }
      val right = rightPos - centerPos match {
        case 0 => Empty
        case 1 => Leaf(rightPos)
        case x:Int => apply(values, centerPos+1, centerPos + (1+rightPos-centerPos)/2, rightPos, clockwise)
      }
      if (clockwise) new Node(centerPos, right, left) else new Node(centerPos, left, right)
    }
  }
}
