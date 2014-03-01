package org.toxicblend.operations.generatemaze

import toxi.geom.ReadonlyVec3D
import toxi.geom.AABB
import toxi.geom.Vec3D
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.protobuf.ToxicBlendProtos.Model.{Builder=>PbBuilder}
import org.toxicblend.typeconverters.Matrix4x4Converter
import scala.util.Random
//import scala.math.Ordering.Implicits._

case class WallIndex(val index:Int)  // just a typesafe int dedicated for Walls
case class NodeIndex(val index:Int)  // just a typesafe int dedicated for Nodes
class Wall(val index:WallIndex, val nodeA:NodeIndex, val nodeB:NodeIndex, val aToB:IndexedSeq[ReadonlyVec3D]) 
class Node(val index:NodeIndex, val walls:ArrayBuffer[WallIndex], val coordinate:ReadonlyVec3D)

class MazeGenerator(val bounds:AABB) {
  val nodes = new ArrayBuffer[Node]
  val walls = new ArrayBuffer[Wall]
  val unusedWalls = new HashSet[Wall] 
  val nodeMap = new HashMap[ReadonlyVec3D,Node]
  
  /**
   * Returns the node at coordinate and a boolean indicating if the node was created or reused.
   * not thread safe.
   */
  protected def getOrCreateNode(coordinate:ReadonlyVec3D) = {
    if (nodeMap.contains(coordinate)) {
      (nodeMap(coordinate),false)
    } else {
      val walls = new ArrayBuffer[WallIndex] 
      val newNode = new Node(new NodeIndex(nodes.size), walls, coordinate)
      nodes.append(newNode)
      nodeMap.put(newNode.coordinate, newNode)
      (newNode,true)
    }
  }
  
  /**
   * finds the nodes closes to the @pointsOfInterest, then pick randomly among the top @randomSelection candidates
   */
  protected def randomCornerNode(pointsOfInterest:IndexedSeq[ReadonlyVec3D], randomSelection:Int):Node = {
    def diff(t2:(Float,Node)) = -t2._1
    val pq = new PriorityQueue[(Float,Node)]()(Ordering.by(diff))
    // ideally i would like to drop the last element for every element i add (once i got more than @randomSelection that is)
    // but the PriorityQueue seems to lack that functionality
    nodes.foreach(n => pq += ((pointsOfInterest.map( v => v.distanceToSquared(n.coordinate)).min, n)) )
    // pick a random corner
    if (pq.size > randomSelection+1) (0 until Random.nextInt(randomSelection)).foreach(_ => pq.dequeue)
    pq.dequeue._2
  }
  
  /**
   * returns a random node close to the AABB corners
   */
  def randomCornerNode:Node = {
    val extentHalf = bounds.getExtent.scale(0.5f)
    val corners = Array(bounds.add(new Vec3D( extentHalf.x,  extentHalf.y,  extentHalf.z)),
                        bounds.add(new Vec3D( extentHalf.x, -extentHalf.y,  extentHalf.z)),
                        bounds.add(new Vec3D(-extentHalf.x, -extentHalf.y,  extentHalf.z)),
                        bounds.add(new Vec3D(-extentHalf.x,  extentHalf.y,  extentHalf.z)),
                        bounds.add(new Vec3D( extentHalf.x,  extentHalf.y, -extentHalf.z)),
                        bounds.add(new Vec3D( extentHalf.x, -extentHalf.y, -extentHalf.z)),
                        bounds.add(new Vec3D(-extentHalf.x, -extentHalf.y, -extentHalf.z)),
                        bounds.add(new Vec3D(-extentHalf.x,  extentHalf.y, -extentHalf.z)))
    val node = randomCornerNode(corners,4)
    println("Picked a corner node at " + node.coordinate + " aabb:" + bounds)
    node
  }
  
  /**
   * returns a random node close to center
   */
  def randomCenterNode:Node = {
    val center = Array(bounds.copy)
    val node = randomCornerNode(center, 4)
    println("Picked a center node at " + node.coordinate + " aabb:" + bounds)
    node
  }
    
  /**
   * not thread safe
   */
  def addWall(coordinates:IndexedSeq[ReadonlyVec3D]) = {
    val (nodeA,nodeAnew) = getOrCreateNode(coordinates(0))
    val (nodeB,nodeBnew) = getOrCreateNode(coordinates.last)
    
    val newWall = new Wall(new WallIndex(walls.size), nodeA.index, nodeB.index, coordinates)
    walls += newWall
    nodeA.walls += newWall.index
    nodeB.walls += newWall.index  
  }
  
  /**
   * 
   * From http://en.wikipedia.org/wiki/Maze_generation_algorithm :
   * 
   * This algorithm is a randomized version of Prim's algorithm.
   * 
   * Start with a grid full of walls.
   * Pick a node, mark it as part of the maze. Add the walls of the node to the wall list.
   * While there are walls in the list:
   *   Pick a random wall from the list. If the node on the opposite side isn't in the maze yet:
   *       Make the wall a passage and mark the node on the opposite side as part of the maze.
   *       Add the neighboring walls of the node to the wall list.
   *     If the node on the opposite side already was in the maze, remove the wall from the list. 
   */
  def reallyGenerateMaze(m3dc:Mesh3DConverter, startPointSetting:StartPoint.Value) = {
    
    /**
     * picks a random WallIndex from a set.
     * TODO: This could probably be improved a lot
     */
    def randomWallIndex(set:Set[WallIndex]):WallIndex = {
      val random = Random.nextInt(set.size)
      var randomWallIndex:WallIndex = null
      val setIterator = set.iterator
      (0 to random).foreach( i => {
        if (i == random) randomWallIndex = setIterator.next
        else setIterator.next
      })
      randomWallIndex
    }
    
    val randomNode:Node = startPointSetting match {
      case StartPoint.CORNER => randomCornerNode
      case StartPoint.CENTER => randomCenterNode
      case StartPoint.RANDOM => nodes(Random.nextInt(nodes.size))
    }
    
    val wallCandidates = new HashSet[WallIndex]
    val unusedNodes = { 
      val set = new HashSet[NodeIndex]
      nodes.foreach(node => set.add(node.index))
      set.remove(randomNode.index)
    }
        
    val partOfTheMaze = new HashSet[NodeIndex]; 
    partOfTheMaze.add(randomNode.index)
    randomNode.walls.foreach(wall => wallCandidates.add(wall))
   
    while (wallCandidates.size > 0) {
      //Pick a random wall from the list.
      val randomWall = walls(randomWallIndex(wallCandidates).index)
      //val fromNodeIndex = randomWall.nodeA
      if (!partOfTheMaze.contains(randomWall.nodeA) || !partOfTheMaze.contains(randomWall.nodeB) ) {
        val oppositeNodeIndex = if (!partOfTheMaze.contains(randomWall.nodeA)) {
          randomWall.nodeA
        } else {
          randomWall.nodeB
        } 
        // If the node on the opposite side isn't in the maze yet:
        //  Make the wall a passage and mark the node on the opposite side as part of the maze.
        
        m3dc.addEdges(randomWall.aToB)
        partOfTheMaze.add(oppositeNodeIndex)
        
        //  Add the neighboring walls of the node to the wall list.
        nodes(oppositeNodeIndex.index).walls.foreach(wallIndex => {
          
          val wall = walls(wallIndex.index)
          //println("Checking  neighboring walls: "+ wallIndex + " nodeA=" + wall.nodeA.index + "  nodeB=" + wall.nodeB.index)
          if (wall.nodeA == oppositeNodeIndex) {
            if (!partOfTheMaze.contains(wall.nodeB)) {
              //println("(nodeB) adding " +  wallIndex + " to the wallCandidates")
              wallCandidates.add(wallIndex)
            }
          } else {
            if (!partOfTheMaze.contains(wall.nodeA)) {
              //println("(nodeA) adding " +  wallIndex + " to the wallCandidates")
              wallCandidates.add(wallIndex)
            }
          }
        })
      }
      //println("removing " +  randomWall.index + " from the wallCandidates")
      wallCandidates.remove(randomWall.index)
    }  
  }
  
  def generateMaze(name:String, matrix:Option[Matrix4x4Converter], startPointSetting:StartPoint.Value):PbBuilder = {
    
    val m3dc = new Mesh3DConverter(name)
    reallyGenerateMaze(m3dc,startPointSetting)
        
    if (matrix.isDefined) {
      m3dc.toPBModel(None, None).setWorldOrientation(matrix.get.toPBModel)
    } else {
      m3dc.toPBModel(None, None)
    }
  }
}