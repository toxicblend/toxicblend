package org.toxicblend.operations.boostmedianaxis

import scala.collection.mutable.ArrayBuffer
import toxi.geom.PointQuadtree
import java.util.ArrayList
import toxi.geom.Vec2D
import toxi.geom.Vec3D
import toxi.geom.AABB
import toxi.geom.Rect
import scala.math.max
import toxi.geom.ReadonlyVec2D
import toxi.geom.ReadonlyVec3D
import scala.math.max
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import org.toxicblend.typeconverters.Mesh3DConverter

/**
 * A helper class that converts a number of connected edges into a Mesh3DConverter
 */
class InteriorEdges( val interiorEdges:Mesh3DConverter ) {
  
  val aabb = new AABB
  interiorEdges.getVertexes.foreach(p => aabb.growToContainPoint(p))
    
  @inline
  def numberOfVertexes():Int = interiorEdges.getVertexes.size
  
  @inline
  def origin():ReadonlyVec2D = {
    new Vec2D(aabb.x, aabb.z)
  }
  
  /**
   * Return the largest of x or y axis
   */
  @inline
  def widthHeight():Float = {
    val extent = aabb.getExtent()
    max(extent.x,extent.y)
  }
  
  /**
   * build an quadtree of points in xy plane. 
   * Every 3d points that share identical x,y components will be mapped in an array
   */
  //protected def populateQuadTree():(PointQuadtree,HashMap[ReadonlyVec2D, ReadonlyVec3D] ) = {
    //println("Creating new PointQuadtree with origin %s width %f".format(origin.toString(), widthHeight))
    //val children = new ArrayBuffer[ReadonlyVec3D]
    /*val qt = {
      val wh = widthHeight
      new PointQuadtree(new Vec2D(aabb.x-wh/2f, aabb.y-wh/2f), wh)
    }
    */
    // create a map{ReadonlyVec2D => Array[ReadonlyVec3D] }

    
    //new HashMap[ReadonlyVec2D, ReadonlyVec3D]
    
    //(qt,map)   
 // }
  
 
    /*interiorEdges.foreach( ie => ie.sliding(2,1).foreach (edge => {
        rv.addEdges(edge)
      })
    )
   
    val (qt,map) = populateQuadTree()   
    val processed = new HashSet[Vec2D]
    //val rv = new ArrayBuffer[Vec2DZ]
    / **
    for(i <- 0 until points.length) yield {
      val point = points(i)
      if (!processed.contains(point)) {
	      val rect = new Rect(point.getComponent(0)-epsilon, point.getComponent(1)-epsilon, epsilon*2.0f, epsilon*2.0f)
	      //println("looking at %s".format(rect.toString()))
	      val neighbours = qt.getPointsWithinRect(rect)
	      if (null==neighbours || 0==neighbours.size) {
	        println("  something is wrong with ringId:%d, no neighbours (%f:%f:%f). point index %d. Maybe your octree is too small? Check edge for gaps".format(ringId, point.getX, point.getY, point.getZ, i))
	      } else {
	        if (neighbours.size==1) {
	          // keep this single point
	          rv+=neighbours.get(0).asInstanceOf[Vec2DZ]
	        } else {  
	          val median = neighbours.get(0) // new Vec2DZ(neighbours.get(0))//.asInstanceOf[Vec2DZ])
		        for (j<-1 until neighbours.size ){
		          val m = neighbours.get(j)// new Vec2DZ(neighbours.get(j))//.asInstanceOf[Vec2DZ])
		          median.addToX(m.getX)
		          median.addToY(m.getY)
		          median.addToZ(m.getZ)
		        }
	          median.divideBy(neighbours.size.toFloat)
	          qt.addPoint(median)
	          rv+=median
	          for (j<-0 until neighbours.size) yield {
		          val p = neighbours.get(j).asInstanceOf[Vec2DZ]
		          if (!processed.contains(p)) {          
			          //println("  will replace (%f,%f,%f) with (%f,%f,%f)".format(p.getX, p.getY, p.getZ, median.getX, median.getY, median.getZ))
			          for(x <- p.edges) yield {
			            if (!processed.contains(x)) {
			              x.edges.remove(p)
			              if (x.ne(median)){
			                x.edges.add(median)
			              } else {
			                //println("strange median points to itself")
			              }
			            }
			          }
			          p.edges.clear
			          processed.add(p)
			          //deleted.add(p)
	            }
		        }
	        }
	      }
	      processed.add(point)
      }
    }
    qt.empty()
    println("removeFloats: Removed %d doubles in ring %d".format(points.length-rv.length, ringId))
    * * /
    * /
    (ringId,rv.toArray) // /
  //}
  }*/
}