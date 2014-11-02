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
                             private val subjectPolygon:Polygon2D, 
                             private val clipPolygon:Polygon2D,
                             private val precision:Double) {
  
  def this(subject:Polygon2D, clip:Polygon2D, precision:Double) = {
    this(new DoubleLinkedList[VertexInfo], new DoubleLinkedList[VertexInfo], subject, clip, precision)  
  }
  
  protected def b2s(b:Boolean) = if (b) "i" else ""
   
  protected def findIntersectons : Boolean = {
    var c2 = clipList.head
    var s2 = subjectList.head
    
    @inline def findIntersections2(c1:DoubleLinkedListElement[VertexInfo]) = {
      
      @inline def findIntersections3(s1:DoubleLinkedListElement[VertexInfo]) = {       
        //println("c=" + c1.data.v + " -> " + c2.data.v  + "   s=" + s1.data.v + " -> " + s2.data.v )
        if (c1.data.v == s1.data.v) {
          //println("same point c: " + c)
          //println("same point s: " + s + "\n")
          //assert(c1.data.otherList.get.eq(s))
          //assert(s.data.otherList.get.eq(c1))
        }
        assert(c1.data.v!=c2.data.v, "c1.data.v == c2.data.v")
        assert(s1.data.v!=s2.data.v, "s1.data.v == s2.data.v")
        val cLine = new FiniteLine2D(c1.data.v, c2.data.v)
        val sLine = new FiniteLine2D(s1.data.v, s2.data.v)
        
        def addOrUpdateIntersection(intersection:Vec2D) = {
          val cElement = if (intersection =~= (cLine.a, precision)) {
            c1
          } else if (intersection =~= (cLine.b, precision)  ) {
            c2
          } else {
            val prevC2 = c2
            c2 = clipList.insertAfter(c1, new VertexInfo(intersection))
            //println("inserted intersection" + intersection + " to c " + c1.data.v + " to " + prevC2.data.v)
            //assert(c2.next.eq(prevC2), "" + c2 + "!=" + prevC2)
            //assert(!(prevI.data.v =~= (c1.data.v, precision)), "" + prevI.data.v  + "==" + c1.data.v)
            c2
          }
          
          val sElement = if (intersection =~= (sLine.a, precision)) {
            s1
          } else if (intersection =~= (sLine.b, precision)) {
            s2
          } else {
            val prevS2 = s2
            s2 = subjectList.insertAfter(s1, new VertexInfo(intersection))
            //println("inserted intersection" + intersection + " to s " + s1.data.v + " to " + prevS2.data.v)
            //assert(s2.next.eq(prevS2), "" + s2 + "!=" + prevS2)
            //assert(!(prevI.data.v =~= (s1.data.v, precision)), "" + prevI.data.v  + "==" + s1.data.v)
            s2
          }
          
          sElement.data.otherList = Option(cElement)
          cElement.data.otherList = Option(sElement)
        }
        
        val intersectionOption = cLine.intersectLine(sLine)
        if (intersectionOption.isDefined) {
          intersectionOption.get match {
            case intersection:SimpleIntersection => addOrUpdateIntersection(intersection.p) 
            case intersection:CoincidentIntersection => {
              //println("CoincidentIntersection " + cLine + " and " + sLine + " intersects at " + intersection.a + " to " + intersection.b)
              addOrUpdateIntersection(intersection.a)
              addOrUpdateIntersection(intersection.b)
            }
          
            //println("added intersection :" + intersection)         
            //println("ci = " + c.data.v + " -> " + c.next.data.v )
            //println("si = " + s.data.v + " -> " + s.next.data.v )
          }
        }
      }
      {
        var s1 = subjectList.head
        if (s1.hasNext) do {
          s2 = s1.next
          findIntersections3(s1)
          s1 = s1.next
        } while (s1.hasNext)
        s2 = subjectList.head
        findIntersections3(s1)
      }
    }
    {
      var c1 = clipList.head
      if (c1.hasNext) do {
        c2 = c1.next
        findIntersections2(c1)
        c1 = c1.next
      } while (c1.hasNext)
      c2 = clipList.head
      findIntersections2(c1)
    }  
    // figure out if we added any intersections
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
   
  protected def selectivelyAddVertices(prevI:DoubleLinkedListElement[VertexInfo], currentI:DoubleLinkedListElement[VertexInfo], clipPolygon:Polygon2D, rv:ArrayBuffer[Vec2D]) {
    //println("selectivelyAddVertices: " + prevI.data.v + " -..-> " + currentI.data.v)
    if (next(prevI, SUBJECT_LIST) == currentI) {
      // no non-intersection in between
      if (rv.size==0 || rv.last!=prevI.data.v) {
        rv.append(prevI.data.v)
        println("1selectivelyAddVertices added" + prevI.data.v )
      }
      rv.append(currentI.data.v)
      println("2selectivelyAddVertices added" + currentI.data.v )
    } else {
      val samplePointS = next(prevI, SUBJECT_LIST)
      if ( clipPolygon.containsPoint(samplePointS.data.v)) {
       //println("samplepoint:" +samplePointS.data.v + " was inside clip. Getting samples from the subject list") 
       addVertices(prevI, currentI, rv, SUBJECT_LIST)
      } else {
        val samplePointC = next(prevI.data.otherList.get, CLIP_LIST)
        if ( subjectPolygon.containsPoint(samplePointC.data.v)) {
          println("samplepoint:" +samplePointS.data.v + " was not inside clip polygon. Getting samples from the clip list")
          println("samplePointC:" +samplePointC.data.v + b2s(samplePointC.data.isIntersection) )
          val nextCIntersection = findNextIntersection(prevI.data.otherList.get, CLIP_LIST)
          addVertices(prevI.data.otherList.get, nextCIntersection, rv, CLIP_LIST)
        } else {
          println("wtf?")
          println("subjectPolygon=" + subjectPolygon.vertices.mkString(","))
          println("clipPolygon=" + clipPolygon.vertices.mkString(","))
          println("generated a bobo")
          val nextCIntersection = findNextIntersection(prevI.data.otherList.get, CLIP_LIST)
        }
      }
    }
  }
  
  /**
   * find segments in between intersection points and determine if the segment is inside or outside the clipping polygon
   */
  protected def filter(clipPolygon:Polygon2D):IndexedSeq[IndexedSeq[Vec2D]] = {
    
    val beginningS = subjectList.firstElement
    
    // find first intersection point
    if (!beginningS.data.isIntersection) {
      // no intersections found, return entire subject polygon
      return IndexedSeq[IndexedSeq[Vec2D]](subjectList.toArray.map(d => d.v))
    }
    //println("beginningS=" + beginningS.data.v + b2s(beginningS.data.isIntersection) )

    val rv = new ArrayBuffer[IndexedSeq[Vec2D]]
    var currentBuffer = new ArrayBuffer[Vec2D]
    var prevS = beginningS
    var currentS = findNextIntersection(prevS, SUBJECT_LIST)
    currentBuffer.append(prevS.data.v)
    if (currentS != beginningS) do {
      //println("prev=" + prevS.data.v + b2s(prevS.data.isIntersection) + " current=" + currentS.data.v + b2s(currentS.data.isIntersection))
      selectivelyAddVertices(prevS, currentS, clipPolygon, currentBuffer)
      prevS = currentS
      currentS = findNextIntersection(currentS, SUBJECT_LIST)
      if (currentBuffer.size >= 1 && currentBuffer.head == currentS.data.v ) {
        currentBuffer.dropRight(1)
        rv.append(currentBuffer)
        currentBuffer = new ArrayBuffer[Vec2D]
        println("switched buffers")
      }
    } while (currentS != beginningS &&  currentS != prevS)
    //currentI = findNextIntersection(next(prevI))
    selectivelyAddVertices(prevS, currentS, clipPolygon, currentBuffer)
    currentBuffer.dropRight(1)
    rv.append(currentBuffer)
    rv
  }
  
  protected def execute:IndexedSeq[IndexedSeq[Vec2D]] = {
   
    val subjectEdges = subjectPolygon.vertices
    val clipEdges = clipPolygon.vertices 
    // Step 1: insert the polygons into linked lists
    subjectEdges.foreach(v => subjectList.append(new VertexInfo(v)))
    clipEdges.foreach(v => clipList.append(new VertexInfo(v)))
        
    //println("1-subject:" + subjectList.map(i => i.data.v.toString + b2s(i.data.isIntersection)).mkString(","))
    //println("1-clip:" + clipList.map(i => i.data.v.toString + b2s(i.data.isIntersection)).mkString(","))
    
    // Step 2: iterate through the lists and insert intersections into both lists    
    if (findIntersectons) {  
      //println("2-subject:" + subjectList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      //println("2-clip:" + clipList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      
      // shift the polygons so that they start with the same intersection
      shiftToFirstIntersection(subjectList, clipList)
      println("3-subject:" + subjectList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      println("3-clip:" + clipList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
        
      // step 3: filter out "in between" intersection segments that are outside the clip polygon
      filter(clipPolygon)
    } else {
      // no intersections found
      if (clipPolygon.containsPoint(subjectEdges.head)) IndexedSeq[IndexedSeq[Vec2D]](subjectEdges)
      else if (subjectPolygon.containsPoint(clipEdges.head)) IndexedSeq[IndexedSeq[Vec2D]](clipEdges)
      else IndexedSeq[IndexedSeq[Vec2D]]()
    }
  }
}

object WeilerAthertonClipper {
  def clip(subject:Polygon2D, clip:Polygon2D, precision:Double=Polygon2D.ε):IndexedSeq[Polygon2D] = 
    new WeilerAthertonClipper(subject, clip, precision).execute.map(p=>Polygon2D(p))
  
/*  def clip(subject:IndexedSeq[Vec2D], clip:IndexedSeq[Vec2D], precision:Double=Polygon2D.ε) = 
    new WeilerAthertonClipper(new Polygon2D(subject), new Polygon2D(clip), precision).execute
*/
}