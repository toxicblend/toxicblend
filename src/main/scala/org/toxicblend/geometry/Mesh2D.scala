package org.toxicblend.geometry

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.IndexedSeqLike
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import toxi.geom.BooleanShapeBuilder
import toxi.geom.{Vec2D=>TVec2D}
import toxi.geom.{Polygon2D=>TPolygon2D}
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.Polygon2D
import org.toxicblend.util.EdgeToFaceMap
import java.awt.geom.GeneralPath
import java.awt.geom.Path2D
import java.awt.geom.Area
import collection.JavaConversions._

/**
 * A container for a 2D half-edge structure. 
 * It contains :
 *  2D vertices indexed by Int.
 *  Faces indexed by Int, containing lists of vertex indexes
 * The faces are implicitly closed, e.g. the face loop [0,1,2,0] is represented as [0,1,2]
 */
class Mesh2D protected ( val vertices:ArrayBuffer[Vec2D], val faces:ArrayBuffer[ArrayBuffer[Int]]){
  
  def this( f:(ArrayBuffer[Vec2D],ArrayBuffer[ArrayBuffer[Int]]) ) = {
    this(f._1,f._2)
  }
  
  protected def uniqueConsecutivePoints(input:ArrayBuffer[Int]) = {
    val output = new ArrayBuffer[Int](input.size)
    var prev:Int = input(0)
    output += prev
    input.drop(1).foreach(i => {
      if (prev != i) 
        output += i
      prev = i
    }) 
    output
  }
  
  /** 
   * Removes duplicated vertices and recalculates the faces
   */
  protected def removeDoubles:Mesh2D = {
    val uniquePoints = new HashMap[Vec2D, Int]
    // translationTable(oldIndex) == newIndex (or -1 if unassigned)
    val translationTable = (0 until vertices.size).map( _ => -1).toArray 
    var pNewIndex=0
    (0 until vertices.size).foreach(pOldIndex => {
      val p = vertices(pOldIndex)
      if (uniquePoints contains p) {
        // p is already known so it is not unique
        pNewIndex = uniquePoints(p)
      } else {
        pNewIndex = uniquePoints.size
        uniquePoints(p) = pNewIndex
      }
      translationTable(pOldIndex) = pNewIndex
    })
    val newVertices = new Array[Vec2D](uniquePoints.size).to[ArrayBuffer]
    (0 until vertices.size).foreach(pOldIndex => {
      newVertices(translationTable(pOldIndex)) = vertices(pOldIndex)
    })
    val newFaces = faces.map( f => uniqueConsecutivePoints(f.map(p => translationTable(p)))).filter(x => x.size>1)
    /*
    println("removeDoubles:")
    println("  newVertices:" + newVertices.mkString("{",",","}"))
    println("  newFaces   :" + newFaces.map(x => x.mkString("(",", ",")")).mkString(", ")) 
    println()
    //(newVertices,fuseFaces(newFaces))
    */ 
    setState(newVertices,newFaces)
    this
  }
  
  /**
   * override the internal state with new vertices and faces
   */
  protected def setState(vs:ArrayBuffer[Vec2D],fs:ArrayBuffer[ArrayBuffer[Int]]) {
    vertices.clear
    vs.foreach( v => vertices += v)
    faces.clear
    fs.foreach( f => faces += f)
  }
    
  /** 
   * Recalculates the faces using BooleanShapeBuilder
   */
  protected def mergeAllFacesWithBooleanShapeBuilder:Mesh2D = {
    
    val builder = new BooleanShapeBuilder(BooleanShapeBuilder.Type.UNION)
    faces.foreach(facePoints => {
      val path=new TPolygon2D() //Path2D.Float(PathIterator.WIND_NON_ZERO, facePoints.size)
      facePoints.foreach(pointIndex => {
        val point = vertices(pointIndex)
        path.add(point.x.toFloat, point.y.toFloat)
      })
      builder.addShape(path)
    })
    buildFromPolygons(builder.computeShapes())
  }
  
  protected def buildFromPolygons(unionPolygons:java.util.List[TPolygon2D]):Mesh2D = { 
    val rvVertices = new ArrayBuffer[TVec2D]()
    val rvFaces = new ArrayBuffer[ArrayBuffer[Int]]()
    unionPolygons.foreach(polygon => {
      if (polygon.size>1) {
        var index = rvVertices.size
        val tmpFaces = new ArrayBuffer[Int]
        polygon.foreach(v => { 
          rvVertices.append(v) 
          tmpFaces.append(index)
          index += 1
        })
        rvFaces.append(tmpFaces)
      }
    })
    
    setState(rvVertices.map(v=>Vec2D(v.x,v.y)), rvFaces)
    this
  }
  
  /**
   * helper method that converts a face to an area object
   */
  protected def poly2Area(index:Int):Area = {
    val thisFace = faces(index)
    val firstVertex = vertices(thisFace(0))
    val gp = new GeneralPath(Path2D.WIND_EVEN_ODD,thisFace.size)
    gp.moveTo(firstVertex.x, firstVertex.y)
    thisFace.foreach(vi => {
      val v = vertices(vi)
      gp.lineTo(v.x, v.y)
    })
    gp.closePath
    new Area(gp)
  } 
  
  /** 
   * Recalculates the faces using a parallel BooleanShapeBuilder
   */
  protected def mergeAllFaces:Mesh2D = {
    val seqOp=(a:Area,b:Int) =>  {val rv = poly2Area(b); rv.add(a); rv }
    val combOp=(a:Area,b:Area) => { a.add(b); a }      
    
    val builder = new BooleanShapeBuilder(BooleanShapeBuilder.Type.UNION)
    builder.combineWithArea((0 until faces.size).par.aggregate(new Area)(seqOp,combOp))
    buildFromPolygons(builder.computeShapes())
  }
    
  def projectionOutline(multiThread:Boolean=false):Mesh2D = {
    removeDoubles
    if (multiThread) {
      mergeAllFaces
    } else {
      mergeAllFacesWithBooleanShapeBuilder
    }
    this
  }
}

object Mesh2D {
  def apply( vertices:ArrayBuffer[Vec2D], faces:ArrayBuffer[ArrayBuffer[Int]]) = {
    new Mesh2D(vertices, faces)
  } 
}