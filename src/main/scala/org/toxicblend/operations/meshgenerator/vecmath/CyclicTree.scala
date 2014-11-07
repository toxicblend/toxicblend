package org.toxicblend.operations.meshgenerator.vecmath

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import org.toxicblend.ToxicblendException

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
  

  protected[vecmath] def 
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

class CyclicTree(val seq:IndexedSeq[Payload], private val tree:Tree, val center:Vec2D, val clockwise:Boolean){
  
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
  
  def getIntersectonPoint(searchAngle:Double, center:Vec2D):Option[Vec2D] = {
    val interval = searchIntervalWithLimits(searchAngle)
    if (interval.isDefined) {
      val i1 = new InfiniteLine2D(center, center.add(Vec2D(searchAngle)))
      val i2 = new InfiniteLine2D(interval.get._1.pos, interval.get._2.pos)
      i1.intersectLine(i2)
    } else {
      None
    }
  }
  
  override def toString = {
    tree.toSequence(seq).mkString(",")
  }
  
  def toSequence = tree.toSequence(seq)
}

object CyclicTree {
    
  /**
   * makes sure the elements of the sequence are in order. Cyclic ordering is not altered
   * @param clockwise: if the ordering is known it can be specified here, if not - use None and it will be calculated
   */
  def inOrder(seq:IndexedSeq[Payload], clockwise:Option[Boolean]=None) : (IndexedSeq[Payload],Boolean) = {
    if (seq.size > 0) {
      val isClockwise = if (clockwise.isDefined) clockwise.get else Polygon2D.isClockwise(seq.map(p=>p.pos))
      //println("inOrder:clockwise=" + isClockwise )
      //println("original=" + seq.map(v=> "@" + v.angle).mkString(","))
          
      if (isClockwise) {
        // clockwise: angles should decrease as we go down the sequence
        val largestAtPos = (0 until seq.size).reduceLeft( (x,a) => if (seq(x).angle>seq(a).angle) x else a )
        if (largestAtPos == 0) {
          (seq,isClockwise)
        } else {
          val rv =  seq.slice(largestAtPos, seq.size) ++ seq.slice(0, largestAtPos)
          //println("clockwise=" + isClockwise + " largestAtPos was " + largestAtPos)
          if (false){
            println("seq=" + seq.map(v=>"" + "@" + v.angle*180d/math.Pi).mkString(","))
            println("rv=" + rv.map(v=>"" + "@" + v.angle*180d/math.Pi).mkString(","))
          }
          if (false) {
            println("seq=" + seq.map(v=>"" + "@" + v.angle).mkString(","))
            println("rv=" + rv.map(v=>"" + "@" + v.angle).mkString(","))
          }
          (0 until rv.size).reduceLeft( (a,x) => if (rv(x).angle>rv(a).angle) throw new ToxicblendException("Non-convex shape?") else x)
          (rv,isClockwise)
        }
      } else {
        // Anti-clockwise: angles should increase as we go down the sequence
        val smallestAtPos = (0 until seq.size).reduceLeft( (x,a) => if (seq(x).angle<seq(a).angle) x else a )
        if (smallestAtPos == 0) {
          (seq,isClockwise)
        } else {
          val rv =  seq.slice(smallestAtPos, seq.size) ++ seq.slice(0, smallestAtPos)
          //println("clockwise=" + isClockwise + " smallestAtPos was " + smallestAtPos)
          if (false){
            println("seq=" + seq.map(v=>"" + "@" + v.angle*180d/math.Pi).mkString(","))
            println("rv=" + rv.map(v=>"" + "@" + v.angle*180d/math.Pi).mkString(","))
          }
          if (false) {
            println("seq=" + seq.map(v=>"" + "@" + v.angle).mkString(","))
            println("rv=" + rv.map(v=>"" + "@" + v.angle).mkString(","))
          }
          (0 until rv.size).reduceLeft( (a,x) => if (rv(x).angle<rv(a).angle) throw new ToxicblendException("Non-convex shape?") else x)
          (rv,isClockwise)
        }
      }
    } else (seq, false)
  }
  
  def apply(convexHull:Polygon2D, center:Vec2D):CyclicTree = {
    
    val vertices = convexHull.vertices.map(p => {
      val v = p.sub(center)
      new Payload(angle=v.heading,distance=v.magnitude,p)
    })
    this.apply(vertices, center=center, inputIsClockwise=Option(convexHull.isClockwise))
  }
  
  def apply(aSeq:IndexedSeq[Payload], center:Vec2D, inputIsClockwise:Option[Boolean]=None):CyclicTree  = {
    val (seq, clockwise) = inOrder(aSeq, inputIsClockwise)
    //println(seq.map(p=>p.angle))
    val rootNode = if (seq.size > 0) {
      val centerPos = (seq.size-1)/2
      val leftPos = 0
      val rightPos = seq.size-1
      apply(seq, leftPos, centerPos, rightPos, clockwise)
    } else {
      Empty
    }
    new CyclicTree(seq, rootNode, center=center, clockwise=clockwise)
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
