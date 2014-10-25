package org.toxicblend.operations.meshgenerator.vecmath

import scala.collection.mutable.ArrayBuffer

object SourceList extends Enumeration {
  type SourceList = Value
  val SUBJECT_LIST, CLIP_LIST, BOTH_LISTS = Value
}
import SourceList._

class VertexInfo(val v:Vec2D, var otherList:Option[DoubleLinkedListElement[VertexInfo]]=None) {
  
  def isIntersection = otherList.isDefined
  
  override def toString = {
    if (v==null) println("v==null")
    if (otherList==null) println("otherList==null")
    //if (isIntersection.eq(null)) println("isIntersection==null")
    //val otherStr = if (otherList.isDefined) otherList.get else ""
    val other = if (otherList.isDefined) "o" else ""
    val intersection = if (isIntersection) "i" else ""
    v.toString + other + intersection
  }
}

class WeilerAthertonClipper( private val subjectList:DoubleLinkedList[VertexInfo], 
                             private val clipList:DoubleLinkedList[VertexInfo], 
                             private val subject:IndexedSeq[Vec2D], 
                             private val clipPolygon:Polygon2D,
                             private val precision:Double) {
  
  def this(subject:IndexedSeq[Vec2D], clip:Polygon2D, precision:Double) = {
    this(new DoubleLinkedList[VertexInfo], new DoubleLinkedList[VertexInfo], subject, clip, precision)  
  }
  
  protected def b2s(b:Boolean) = if (b) "i" else ""
  
 
  protected def findIntersectons : Boolean = {
    var c1 = clipList.head
    var s1 = subjectList.head
    
    @inline def findIntersections2(c2:DoubleLinkedListElement[VertexInfo]) = {
      
      @inline def findIntersections3(s2:DoubleLinkedListElement[VertexInfo]) = {
         
        //println("c=" + c1.data.v + " -> " + c2.data.v  + "   s=" + s1.data.v + " -> " + s2.data.v )
        if (c1.data.v == s1.data.v) {
          //println("same point c: " + c)
          //println("same point s: " + s + "\n")
          //assert(c1.data.otherList.get.eq(s))
          //assert(s.data.otherList.get.eq(c1))
        }
        val cLine = new Line2D(c1.data.v, c2.data.v)
        val sLine = new Line2D(s1.data.v, s2.data.v)
        val intersectionOption = cLine.intersectLine(sLine)
        if (intersectionOption.isDefined) {
          val intersection = intersectionOption.get
          
          val cElement = if (intersection =~= (cLine.a, precision)) {
            c1
          } else if (intersection =~= (cLine.b, precision)  ) {
            c2
          } else {
            val prevI = c1
            c1 = clipList.insertAfter(c1, new VertexInfo(intersection))
            //println("added intersection" + intersection + "to c")
            assert(prevI.next.eq(c1), "" + prevI.next + "!=" + c1)
            assert(!(prevI.data.v =~= (c1.data.v, precision)), "" + prevI.data.v  + "==" + c1.data.v)
            c1
          }
          
          val sElement = if (intersection =~= (sLine.a, precision)) {
            s1
          } else if (intersection =~= (sLine.b, precision)) {
            s2
          } else {
            val prevI = s1
            s1 = subjectList.insertAfter(s1, new VertexInfo(intersection))
            //println("added intersection" + intersection + "to s")
            assert(prevI.next.eq(s1), "" + prevI.next + "!=" + s1)
            assert(!(prevI.data.v =~= (s1.data.v, precision)), "" + prevI.data.v  + "==" + s1.data.v)
            s1 
          }
          
          sElement.data.otherList = Option(cElement)
          cElement.data.otherList = Option(sElement)
          //println("added intersection :" + intersection)
          
          
          //println("ci = " + c.data.v + " -> " + c.next.data.v )
          //println("si = " + s.data.v + " -> " + s.next.data.v )
        }
      }
      
      s1 = subjectList.head
      if (s1.hasNext) do {
        findIntersections3(s1.next)
        s1 = s1.next
      } while (s1.hasNext)
      findIntersections3(subjectList.head)
    
    }

    c1 = clipList.head
    if (c1.hasNext) do {
      findIntersections2(c1.next)
      c1 = c1.next
    } while (c1.hasNext)
    findIntersections2(clipList.head)
    if (clipList.size < subjectList.size) {
      clipList.foreach(c => if (c.data.isIntersection) return true)
    } else {
      subjectList.foreach(s => if (s.data.isIntersection) return true)
    }
    false
  }
  
  protected def shiftToFirstIntersection(list1:DoubleLinkedList[VertexInfo], list2:DoubleLinkedList[VertexInfo]):Boolean = {
    var list1element = list1.head
    var tmp = list1element
    if (!list1element.data.isIntersection) do {
      list1element = list1element.next
    } while (list1element!=null && !list1element.data.isIntersection)
    if (list1element==null) return false
    if (list1element!=list1.head) do {
      list1.moveHeadToLast
    } while (list1element!=list1.head)
    val list2element = list1element.data.otherList.get
    if (!list2element.eq(list2.head)) do {
      //println("shifting:" + list2.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      list2.moveHeadToLast
      //println("result:" + list2.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
    } while (!list2element.eq(list2.head))
    true
  }
  
  protected def next(current:DoubleLinkedListElement[VertexInfo], sourceList:SourceList) : DoubleLinkedListElement[VertexInfo] = {
    sourceList match {
      case SUBJECT_LIST => {
        if (current.hasNext) current.next 
        else if (subjectList.last.eq(current)) subjectList.firstElement
        else { assert(false, "next (subjectList) fail, element not part of list"); null }
      }
      case CLIP_LIST => {
        if (current.hasNext) current.next 
        else if (clipList.last.eq(current)) clipList.firstElement
        else { assert(false, "next (clipList) fail, element not part of list"); null }
      }
      case BOTH_LISTS => {
        if (current.hasNext) current.next 
        else if (clipList.last.eq(current)) clipList.firstElement
        else if (subjectList.last.eq(current)) subjectList.firstElement
        else { assert(false, "next (both lists) fail, element not part of any list"); null }
      }
    } 
  }
  
  protected def findNextIntersection(beginning:DoubleLinkedListElement[VertexInfo], sourceList:SourceList) = {
    var current = next(beginning, sourceList) 
    if (!current.data.isIntersection) do {
      current = next(current, sourceList)
    } while ( !(current.data.isIntersection || current==beginning))
    current
  }
  
  protected def addVertices(first:DoubleLinkedListElement[VertexInfo], last:DoubleLinkedListElement[VertexInfo], rv:ArrayBuffer[Vec2D], sourceList:SourceList) {
    var current = first
    if (rv.size==0 || rv.last!=current.data.v) {
      rv.append(current.data.v)
      //println("addVertices added" + current.data.v + " " + sourceList)
    }
    do {
      current = next(current,sourceList)
      rv.append(current.data.v)
      //println("addVertices added" + current.data.v + " " + sourceList)
    } while (current != last)
  }
  
  /*protected def isInside(first:DoubleLinkedListElement[VertexInfo], last:DoubleLinkedListElement[VertexInfo], clipPolygon:Polygon2D) = {
    var current = first
    do {
      current = current.next
    } while (current != last)
    false
  }*/
  
  protected def selectivelyAddVertices(prevI:DoubleLinkedListElement[VertexInfo], currentI:DoubleLinkedListElement[VertexInfo], clipPolygon:Polygon2D, rv:ArrayBuffer[Vec2D]) {
    //println("selectivelyAddVertices: " + prevI.data.v + " -..-> " + currentI.data.v)
    if (prevI.next == currentI) {
      // no non-intersection in between
      if (rv.size==0 || rv.last!=prevI.data.v) {
        rv.append(prevI.data.v)
        //println("selectivelyAddVertices added" + prevI.data.v )
      }
      rv.append(currentI.data.v)
      //println("selectivelyAddVertices added" + currentI.data.v )
    } else {
     if ( clipPolygon.containsPoint(next(prevI, SUBJECT_LIST).data.v) ) addVertices(prevI, currentI, rv, SUBJECT_LIST)
     else addVertices(prevI.data.otherList.get, currentI.data.otherList.get, rv, CLIP_LIST)
    }
  }
  
  /**
   * find segments in between intersection points and determine if the segment is inside or outside the clipping polygon
   */
  protected def filter(clipPolygon:Polygon2D):IndexedSeq[Vec2D] = {
    
    val beginningI = subjectList.firstElement
    
    // find first intersection point
    if (!beginningI.data.isIntersection) {
      // no intersections found, return entire subject polygon
      return subjectList.toArray.map(d => d.v)
    }
    
    val rv = new ArrayBuffer[Vec2D]
    var prevI = beginningI
    var currentI = beginningI
    rv.append(currentI.data.v)
    do {
      currentI = findNextIntersection(currentI, SUBJECT_LIST)
      selectivelyAddVertices(prevI, currentI, clipPolygon, rv)
      prevI = currentI
    } while (currentI.data.v != beginningI.data.v)
    //currentI = findNextIntersection(next(prevI))
    //selectivelyAddVertices(prevI, currentI, clipPolygon, rv)
    rv.dropRight(1)
  }
  
  protected def execute:IndexedSeq[Vec2D] = {
   
    val clipEdges = clipPolygon.vertices 
    // Step 1: insert the polygons into linked lists
    subject.foreach(v => subjectList.append(new VertexInfo(v)))
    clipEdges.foreach(v => clipList.append(new VertexInfo(v)))
        
    //println("1-subject:" + subjectList.map(i => i.data.v.toString + b2s(i.data.isIntersection)).mkString(","))
    //println("1-clip:" + clipList.map(i => i.data.v.toString + b2s(i.data.isIntersection)).mkString(","))
    
    // Step 2: iterate through the lists and insert intersections into both lists    
    if (findIntersectons) {  
      //println("2-subject:" + subjectList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      //println("2-clip:" + clipList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      
      // shift the polygons so that they start with the same intersection
      shiftToFirstIntersection(subjectList, clipList)
      //println("3-subject:" + subjectList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      //println("3-clip:" + clipList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
        
      // step 3: filter out "in between" intersection segments that are outside the clip polygon
      filter(clipPolygon)
    } else {
      // no intersections found
      if (clipPolygon.containsPoint(subject.head)) subject
      else clipPolygon.vertices 
    }
  }
}

object WeilerAthertonClipper {
  def clip(subject:IndexedSeq[Vec2D], clipPolygon:Polygon2D, precision:Double) = 
    new WeilerAthertonClipper(subject, clipPolygon, precision).execute
}

object WeilerAthertonClipperTest extends App {
  //val subject = ArrayBuffer((50,150),(200,50),(350,150),(350,300),(250,300),(200,250),(150,350),(100,250),(100,200)).map(p=>ImmutableVertex2D(p._1,p._2))
  //val clipEdges = ArrayBuffer((100,100),(300,100),(300,300),(100,300)).map(p=>ImmutableVertex2D(p._1,p._2))
  val subject = {
    val b = new ArrayBuffer[Vec2D]
    (0 until 100).foreach(i => b.append(ImmutableVec2D(100d*math.sin(i*math.Pi/50d), 100d*math.cos(i*math.Pi/50d)) ))
    b
  }
  val clipEdges = ArrayBuffer((0, 0), (100,0), (100,100), (0,100)).map(p=>ImmutableVec2D(p._1,p._2))
  //val subject = ArrayBuffer((0,1),(1,-1),(-1,-1)).map(p=>new Vertex2D(p._1,p._2))
  //val clipEdges = ArrayBuffer((-1, 1),(1,1),(0,-1)).map(p=>new Vertex2D(p._1,p._2))
  println("subject:" + subject.mkString(","))
  println("clipEdges:" + clipEdges.mkString(","))
  
  val clipped = WeilerAthertonClipper.clip(subject,new Polygon2D(clipEdges), 0.00001d)
  
  println("clipped.size=" + clipped.size + "\n" + clipped.mkString("\n"))
}