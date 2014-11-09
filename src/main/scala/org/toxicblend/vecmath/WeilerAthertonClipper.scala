package org.toxicblend.vecmath

import scala.collection.mutable.ArrayBuffer
import scala.IndexedSeq

object SourceList extends Enumeration {
  type SourceList = Value
  val SUBJECT_LIST, CLIP_LIST, BOTH_LISTS = Value
}

import SourceList.{SourceList, SUBJECT_LIST, CLIP_LIST, BOTH_LISTS}

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
          //println("same point c: " + c1)
          //println("same point s: " + s1 + "\n")
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
  
  /**
   * Adds vertices until the next intersection is detected. Then that element is returned as a subject list element
   * The vertex of the last intersection will not be added to the result buffer
   */
  protected def addVerticesUntilNextIntersection(first:DoubleLinkedListElement[VertexInfo], rv:ArrayBuffer[Vec2D], sourceList:SourceList):DoubleLinkedListElement[VertexInfo] = {
    var current = first
    conditionalAppend(rv,current.data.v)
    do {
      current = next(current,sourceList)
      conditionalAppend(rv,current.data.v)
      //println("addVertices added" + current.data.v + " " + sourceList)
    } while (!current.data.isIntersection)
    sourceList match {
      case SUBJECT_LIST => current
      case CLIP_LIST => current.data.otherList.get
    }
  }
  
  /**
   * prevSI: previous subject intersection
   */
  protected def selectivelyAddVertices(prevSI:DoubleLinkedListElement[VertexInfo], rv:ArrayBuffer[Vec2D]):(DoubleLinkedListElement[VertexInfo],Boolean) = {
    //println("selectivelyAddVertices: " + prevSI.data.v )
    val nextS = next(prevSI, SUBJECT_LIST)
    if (nextS.data.isIntersection) {
      // no non-intersection in between
      conditionalAppend(rv,prevSI.data.v)
      conditionalAppend(rv,nextS.data.v)
      //println("2selectivelyAddVertices added" + nextS.data.v )
      (nextS,false)
    } else {
      if ( clipPolygon.containsPoint(nextS.data.v)) {
        //println("samplepoint:" +samplePointS.data.v + " was inside clip. Getting samples from the subject list") 
        val nextSIntersection = addVerticesUntilNextIntersection(prevSI, rv, SUBJECT_LIST)
        if (nextSIntersection.data.v == rv.head) {
          //println("nextSIntersection.data.v == rv.head -> fail")
          (nextSIntersection,true)
        }
        else (nextSIntersection,false)
      } else {
        val samplePointC = next(prevSI.data.otherList.get, CLIP_LIST)
        if ( subjectPolygon.containsPoint(samplePointC.data.v)) {
          //println("samplepointS:" +nextS.data.v + " was not inside clip polygon. Getting samples from the clip list")
          //println("samplePointC:" +samplePointC.data.v + b2s(samplePointC.data.isIntersection) )
          val nextSIntersection = addVerticesUntilNextIntersection(prevSI.data.otherList.get, rv, CLIP_LIST)
          //println("nextSIntersection:" +nextSIntersection.data.v + b2s(nextSIntersection.data.isIntersection) )
          //println("rv:" + rv)

          if (nextSIntersection.data.v == rv.head) {
            //println("nextSIntersection.data.v == rv.head -> fail")
            (findNextIntersection(prevSI,SUBJECT_LIST),true)
          } else {
            (nextSIntersection,false)
          }
        } else {
          //println("wtf?")
          //println("subjectPolygon=" + subjectPolygon.vertices.mkString(","))
          //println("clipPolygon=" + clipPolygon.vertices.mkString(","))
          //println("generated a bobo, list should end here")
          val nextCIntersection = findNextIntersection(prevSI.data.otherList.get, CLIP_LIST)
          (nextCIntersection.data.otherList.get,true)
        }
      }
    }
  }
  
  def countNumberOfIntersections = {
    val firstElement = subjectList.firstElement
    var currentS = firstElement
    var numberOfIntersections = 0
    do {
      currentS = findNextIntersection(currentS, SUBJECT_LIST)
      numberOfIntersections += 1
    } while (currentS!=firstElement)
    numberOfIntersections
  }
  
  /**
   * find segments in between intersection points and determine if the segment is inside or outside the clipping polygon
   */
  protected def filter:IndexedSeq[IndexedSeq[Vec2D]] = {
    
    val usedSubjectIntersections = new collection.mutable.HashSet[DoubleLinkedListElement[VertexInfo]]
    val numberOfIntersections = countNumberOfIntersections
    val firstElement = subjectList.firstElement
    var currentS = firstElement
    var loopBeginningS = firstElement
    var loopBeginningC = loopBeginningS.data.otherList.get
    var fail = false
    var usedIntersections = 0
    val rv = new ArrayBuffer[IndexedSeq[Vec2D]]
    do {
      //println("*1-loopBeginningC=" + loopBeginningC)
      val currentBuffer = new ArrayBuffer[Vec2D]
      
      // find an unused intersection point 
      if (usedSubjectIntersections.contains(loopBeginningS)) do {
        loopBeginningC = findNextIntersection(loopBeginningC, CLIP_LIST)
        loopBeginningS = loopBeginningC.data.otherList.get
      } while (usedSubjectIntersections.contains(loopBeginningS))
      
      //println("*2-loopBeginning=" + loopBeginning)
        
      // Iterate though the subject loop
      currentS = loopBeginningS
      do {
        val (s,f) = selectivelyAddVertices(currentS, currentBuffer)
        /*if (usedSubjectIntersections.contains(currentS)) {
          println("rv=" + rv.mkString(","))
          println("currentBuffer=" + currentBuffer.mkString(","))
          println("loopBeginningS=" + loopBeginningS)
          println("currentS=" + currentS)
          //assert(!usedSubjectIntersections.contains(currentS), "" + currentS + " was already used")
        }*/
        usedSubjectIntersections.add(currentS)
        usedIntersections += 1
        currentS = s   
        fail = f
        //println("*3-currentS=" + currentS)
      } while ((!fail) && !usedSubjectIntersections.contains(currentS))
      // remove last element if it's the same as the head (why is it added in the first place?)
      if (currentBuffer.size>1 && currentBuffer.last == currentBuffer.head) currentBuffer.remove(currentBuffer.size-1)
      if (currentBuffer.size>0) rv.append(currentBuffer)
      loopBeginningC = findNextIntersection(loopBeginningC, CLIP_LIST)
      loopBeginningS = loopBeginningC.data.otherList.get
    } while (numberOfIntersections != usedIntersections)
    rv
  }
  
  @inline protected def conditionalAppend(buffer:ArrayBuffer[Vec2D], v:Vec2D) = if (buffer.size==0 || buffer.last!=v) buffer.append(v)  
  
  /**
   * find segments in between intersection points and determine if the segment is inside or outside the clipping polygon
   */
  protected def malfunctionFilter:IndexedSeq[IndexedSeq[Vec2D]] = {
    
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
    currentBuffer.append(prevS.data.v)
    do {
      //println("prev=" + prevS.data.v + b2s(prevS.data.isIntersection) + " current=" + currentS.data.v + b2s(currentS.data.isIntersection))
      val (currentS,fail) = selectivelyAddVertices(prevS, currentBuffer)
      prevS = currentS
      //currentS = findNextIntersection(currentS, SUBJECT_LIST)
      if (fail) {
        rv.append(currentBuffer)
        currentBuffer = new ArrayBuffer[Vec2D]
        println("switched buffers")
      }
    } while (prevS != beginningS)
    //currentI = findNextIntersection(next(prevI))
    selectivelyAddVertices(prevS, currentBuffer)
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
      //println("3-subject:" + subjectList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
      //println("3-clip:" + clipList.map(i => i.data.v.toIntString + b2s(i.data.isIntersection)).mkString(","))
        
      // step 3: filter out "in between" intersection segments that are outside the clip polygon
      filter
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