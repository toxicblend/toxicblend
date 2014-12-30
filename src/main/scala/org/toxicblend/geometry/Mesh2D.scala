package org.toxicblend.geometry

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.IndexedSeqLike
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import toxi.geom.BooleanShapeBuilder
import toxi.geom.{ Vec2D => TVec2D }
import toxi.geom.{ Polygon2D => TPolygon2D }
import org.toxicblend.vecmath.Vec2D
import org.toxicblend.vecmath.AABB2D
import org.toxicblend.vecmath.Polygon2D
import org.toxicblend.util.EdgeToFaceMap
import org.toxicblend.util.Time.time
import java.awt.geom.GeneralPath
import java.awt.geom.Path2D
import java.awt.geom.Area
import collection.JavaConversions._
import akka.actor.ActorSystem
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.dispatch.MessageDispatcher
import akka.util.Timeout

/**
 * A container for a 2D half-edge structure.
 * It contains :
 *  2D vertices indexed by Int.
 *  Faces indexed by Int, containing lists of vertex indexes
 * The faces are implicitly closed, e.g. the face loop [0,1,2,0] is represented as [0,1,2]
 */
class Mesh2D protected (val vertices: ArrayBuffer[Vec2D], val faces: ArrayBuffer[ArrayBuffer[Int]]) {

  def this(f: (ArrayBuffer[Vec2D], ArrayBuffer[ArrayBuffer[Int]])) = {
    this(f._1, f._2)
  }

  protected def uniqueConsecutivePoints(input: ArrayBuffer[Int]) = {
    val output = new ArrayBuffer[Int](input.size)
    var prev: Int = input(0)
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
  protected def removeDoubles: Mesh2D = {
    val uniquePoints = new HashMap[Vec2D, Int]
    // translationTable(oldIndex) == newIndex (or -1 if unassigned)
    val translationTable = (0 until vertices.size).map(_ => -1).toArray
    var pNewIndex = 0
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
    val newFaces = faces.map(f => uniqueConsecutivePoints(f.map(p => translationTable(p)))).filter(x => x.size > 1)
    setState(newVertices, newFaces)
    this
  }

  /**
   * override the internal state with new vertices and faces
   */
  protected def setState(vs: ArrayBuffer[Vec2D], fs: ArrayBuffer[ArrayBuffer[Int]]) {
    vertices.clear
    vs.foreach(v => vertices += v)
    faces.clear
    fs.foreach(f => faces += f)
  }

  /**
   * Recalculates the faces using BooleanShapeBuilder
   */
  protected def mergeAllFacesWithBooleanShapeBuilder: Mesh2D = {

    val builder = new BooleanShapeBuilder(BooleanShapeBuilder.Type.UNION)
    faces.foreach(facePoints => {
      val path = new TPolygon2D() //Path2D.Float(PathIterator.WIND_NON_ZERO, facePoints.size)
      facePoints.foreach(pointIndex => {
        val point = vertices(pointIndex)
        path.add(point.x.toFloat, point.y.toFloat)
      })
      builder.addShape(path)
    })
    buildFromPolygons(builder.computeShapes())
  }

  protected def buildFromPolygons(unionPolygons: java.util.List[TPolygon2D]): Mesh2D = {
    val rvVertices = new ArrayBuffer[TVec2D]()
    val rvFaces = new ArrayBuffer[ArrayBuffer[Int]]()
    unionPolygons.foreach(polygon => {
      if (polygon.size > 2) {
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

    setState(rvVertices.map(v => Vec2D(v.x, v.y)), rvFaces)
    this
  }

  /**
   * helper method that converts a face to an area object
   */
  protected def poly2Area(index: Int): Area = {
    val thisFace = faces(index)
    val firstVertex = vertices(thisFace(0))
    val gp = new GeneralPath(Path2D.WIND_EVEN_ODD, thisFace.size)
    gp.moveTo(firstVertex.x, firstVertex.y)
    thisFace.foreach(vi => {
      val v = vertices(vi)
      gp.lineTo(v.x, v.y)
    })
    gp.closePath
    new Area(gp)
  }

  protected def quadSum(implicit ec: MessageDispatcher, f0: Future[Area], f1: Future[Area], f2: Future[Area], f3: Future[Area]) = {

    val e1 = for (
      a <- f0.mapTo[Area];
      b <- f1.mapTo[Area];
      c <- Future({ a.add(b); a })
    ) yield c
    val e2 = for (
      a <- f2.mapTo[Area];
      b <- f3.mapTo[Area];
      c <- Future({ a.add(b); a })
    ) yield c
    val e = for (
      a <- e1.mapTo[Area];
      b <- e2.mapTo[Area];
      c <- Future({ a.add(b); a })
    ) yield c
    e
  }

  /**
   * Recalculates the faces using a parallel BooleanShapeBuilder
   */
  protected def mergeAllFaces(actorSystem: ActorSystem): Mesh2D = {

    implicit val ec = actorSystem.dispatchers.defaultGlobalDispatcher

    val seqOp = (area: Area, b: IndexedSeq[Int]) => {
      for (i <- 0 until b.size) area.add(poly2Area(b(i)))
      area
    }
    val combOp = (a: Area, b: Area) => { a.add(b); a }

    val aabb = if (vertices.size > 50000) AABB2D.parallelConstructor(vertices)
    else AABB2D(vertices)

    val BUCKETS = 4 // BUCKETS*BUCKETS is the number of buckets we divide the input into
    val futures = {
      val buckets = new Array[ArrayBuffer[Int]](BUCKETS * BUCKETS)
      for (i <- 0 until buckets.size) buckets(i) = new ArrayBuffer[Int]

      for (f <- 0 until faces.size) {
        val v = vertices(faces(f).head)
        val xb = ((BUCKETS - 1).toDouble * (v.x - aabb.min.x) / aabb.width).toInt
        val yb = ((BUCKETS - 1).toDouble * (v.y - aabb.min.y) / aabb.height).toInt
        //println("placing " + v + " in bucket (" + xb + "," + yb + ")")
        buckets(xb + yb * BUCKETS).append(f)
      }
      buckets.map(b => Future { val area = new Area; for (i <- 0 until b.size) area.add(poly2Area(b(i))); area })
    }
    val futureArea = quadSum(ec, quadSum(ec, futures(0), futures(1), futures(4), futures(5)),
      quadSum(ec, futures(2), futures(3), futures(6), futures(7)),
      quadSum(ec, futures(8), futures(9), futures(12), futures(13)),
      quadSum(ec, futures(10), futures(11), futures(14), futures(15)))

    val builder = new BooleanShapeBuilder(BooleanShapeBuilder.Type.UNION)
    implicit val timeout = Timeout(24 hours)
    val area = Await.result(futureArea, timeout.duration).asInstanceOf[Area]
    builder.combineWithArea(area)
    //builder.combineWithArea(buckets.flatten.par.aggregate(new Area)(seqOp,combOp))  //(0 until faces.size).par.aggregate(new Area)(seqOp,combOp))
    buildFromPolygons(builder.computeShapes())
  }

  def projectionOutline(actorSystem: ActorSystem, multiThread: Boolean = false): Mesh2D = {
    removeDoubles
    if (multiThread) {
      mergeAllFaces(actorSystem)
    } else {
      mergeAllFacesWithBooleanShapeBuilder
    }
    this
  }
}

object Mesh2D {
  def apply(vertices: ArrayBuffer[Vec2D], faces: ArrayBuffer[ArrayBuffer[Int]]) = {
    new Mesh2D(vertices, faces)
  }
}