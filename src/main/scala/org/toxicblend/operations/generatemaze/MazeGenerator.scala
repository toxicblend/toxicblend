package org.toxicblend.operations.generatemaze

import toxi.geom.ReadonlyVec3D
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.protobuf.ToxicBlendProtos.Model.{Builder=>PbBuilder}
import org.toxicblend.typeconverters.Matrix4x4Converter
import scala.util.Random

case class WallIndex(val index:Int)  // just a typesafe int dedicated for Walls
case class NodeIndex(val index:Int)  // just a typesafe int dedicated for Nodes
class Wall(val index:WallIndex, val nodeA:NodeIndex, val nodeB:NodeIndex, val aToB:IndexedSeq[ReadonlyVec3D]) 
class Node(val index:NodeIndex, val walls:ArrayBuffer[WallIndex])

class MazeGenerator() {
  val nodes = new ArrayBuffer[Node]
  val walls = new ArrayBuffer[Wall]
  val unusedWalls = new HashSet[Wall] 
  val nodeMap = new HashMap[ReadonlyVec3D,Node]
  
  /**
   * not thread safe
   */
  protected def getOrCreateNode(coordinate:ReadonlyVec3D) = {
    if (nodeMap.contains(coordinate)) {
      (nodeMap(coordinate),false)
    } else {
      val walls = new ArrayBuffer[WallIndex] 
      val newNode = new Node(new NodeIndex(nodes.size), walls)
      nodes.append(newNode)
      nodeMap.put(coordinate, newNode)
      (newNode,true)
    }
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
  
  def reallyGenerateMaze(m3dc: Mesh3DConverter) = {
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
    
    /**
     * picks a random WallIndex from a set.
     * TODO: This could probably be improved a lot
     */
    def randomWallIndex(set:HashSet[WallIndex]):WallIndex = {
      val random = Random.nextInt(set.size)
      var randomWallIndex:WallIndex = null
      val setIterator = set.iterator
      (0 to random).foreach( i => {
        if (i == random) randomWallIndex = setIterator.next
        else setIterator.next
      })
      //println("picked random wall index: " + randomWallIndex.index + " out of " + set)
      randomWallIndex
    }
    
    var randomNode = nodes(Random.nextInt(nodes.size))
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
        
        m3dc.addMultipleEdges(randomWall.aToB)
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
  
  def generateMaze(name:String, matrix:Option[Matrix4x4Converter]):PbBuilder = {
    
    val m3dc = new Mesh3DConverter(name)
    reallyGenerateMaze(m3dc)
        
    if (matrix.isDefined) {
      m3dc.toPBModel(None, None).setWorldOrientation(matrix.get.toPBModel)
    } else {
      m3dc.toPBModel(None, None)
    }
  }
}