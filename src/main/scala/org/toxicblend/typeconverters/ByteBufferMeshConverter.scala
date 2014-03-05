package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import javax.vecmath.Vector3d
import javax.vecmath.Point3d
import javax.vecmath.Tuple3d
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.JavaConversions._
import org.toxicblend.ToxicblendException
import com.bulletphysics.linearmath.AABB
import com.bulletphysics.linearmath.Matrix4dE
import com.bulletphysics.linearmath.Triangle

class ByteBufferMeshConverter(val totalVerts:Int, val totalTriangles:Int, val gVertices:ByteBuffer, val gIndices:ByteBuffer, val aabb:AABB, val name:String) {
  
  @inline
  def readVertice(result:Tuple3d, index:Int) = {
    result.x = gVertices.getDouble((index*3 + 0) * ByteBufferMeshConverter.VERTEX_SIZE)
    result.y = gVertices.getDouble((index*3 + 1) * ByteBufferMeshConverter.VERTEX_SIZE)
    result.z = gVertices.getDouble((index*3 + 2) * ByteBufferMeshConverter.VERTEX_SIZE)
  }
  
  /**
   * Don't try to modify any vertices in the middle of collision detection
   */
  @inline
  def writeVertice(vertice:Tuple3d, index:Int) = {
    gVertices.putDouble((index*3 + 0) * ByteBufferMeshConverter.VERTEX_SIZE, vertice.x)
    gVertices.putDouble((index*3 + 1) * ByteBufferMeshConverter.VERTEX_SIZE, vertice.y)
    gVertices.putDouble((index*3 + 2) * ByteBufferMeshConverter.VERTEX_SIZE, vertice.z)
  }
  
  @inline
  def readTriangle(triangle:Triangle, triangleIndex:Int) = {
    var vertexIndex = gIndices.getInt((triangleIndex*3 + 0) * ByteBufferMeshConverter.T_INDEX_SIZE)
    readVertice(triangle.a, vertexIndex)
    vertexIndex = gIndices.getInt((triangleIndex*3 + 1) * ByteBufferMeshConverter.T_INDEX_SIZE)
    readVertice(triangle.b, vertexIndex)
    vertexIndex = gIndices.getInt((triangleIndex*3 + 2) * ByteBufferMeshConverter.T_INDEX_SIZE)
    readVertice(triangle.c, vertexIndex)
  }
  
  @inline
  def readTriangle(result:Array[Int], triangleIndex:Int) = {
    result(0) = gIndices.getInt((triangleIndex*3 + 0) * ByteBufferMeshConverter.T_INDEX_SIZE)
    result(1) = gIndices.getInt((triangleIndex*3 + 1) * ByteBufferMeshConverter.T_INDEX_SIZE)
    result(2) = gIndices.getInt((triangleIndex*3 + 2) * ByteBufferMeshConverter.T_INDEX_SIZE)
  }
  
  /**
   * transform each vertex in the ByteBuffer and the AABB
   */
  def transformVertices(m:Matrix4dE) = {
    val point = new Point3d
    (0 until totalVerts).foreach(index => {
      readVertice(point,index)
      m.transform(point)
      writeVertice(point,index)
    })
    m.transform(aabb)
  }
}

object ByteBufferMeshConverter {
  val VERTEX_SIZE = 8 // SizeOf(Double)
  val T_INDEX_SIZE = 4 // SizeOf(Int)
  val VERTEX_STRIDE = 3 * VERTEX_SIZE
  val T_INDEX_STRIDE = 3 * T_INDEX_SIZE
  
  /** 
   * Constructs from a packet buffer model, if there is a world transformation it will be used to calculate the 'real' vertices
   * @param pbModel the model we are reading from
   * @param useWorldCoordinares convert coordinates in the pbModel into world coordinates (apply world transformation)
   * @param unitScale 'extra' scaling needed to convert from one unit of measure to another (e.g. meter to millimeter)
   */
  def apply(pbModel:Model, useWorldCoordinares:Boolean, unitScale:Float):ByteBufferMeshConverter = {
    val totalVerts = pbModel.getVerticesCount
    val totalTriangles = pbModel.getFacesCount
    
    val hasWorldTransformation = pbModel.hasWorldOrientation()
    val worldTransformation = if (hasWorldTransformation) Option(Matrix4dConverter(pbModel.getWorldOrientation())) else None
    
    val gVertices = ByteBuffer.allocateDirect(totalVerts*VERTEX_STRIDE).order(ByteOrder.nativeOrder());
    val gIndices = ByteBuffer.allocateDirect(totalTriangles*T_INDEX_STRIDE).order(ByteOrder.nativeOrder());
    
    val aabb = {
      // initiate aabb with a sample from the first vertex
      val firstVertex = getFirstVertex(pbModel)  
      if (firstVertex.isDefined) {
        val p = firstVertex.get
        if (hasWorldTransformation) {
          worldTransformation.get.matrix.transform(p)
        }
        new AABB(p)
      } else {
        new AABB
      }
    } 
    {
      // fill gVertices with data
      var index = 0
      val point = new Point3d
      val matrix = {
        if (hasWorldTransformation) {
          Option(worldTransformation.get.matrix)
        } else {
          None
        }
      }
      pbModel.getVerticesList.foreach( v => {
        point.set(v.getX,v.getY,v.getZ)
        if (hasWorldTransformation)
          matrix.get.transform(point)
        aabb.aabbExpand(point)
        gVertices.putDouble((index*3 + 0) * VERTEX_SIZE, point.x)
        gVertices.putDouble((index*3 + 1) * VERTEX_SIZE, point.y)
        gVertices.putDouble((index*3 + 2) * VERTEX_SIZE, point.z)
        index+=1
      })
    }
    {
      // fill gIndices with data
      var triangleIndex = 0
      pbModel.getFacesList.foreach(f => {
        if (f.getVerticesCount > 3 ) 
          throw new ToxicblendException("JBullet mesh must be triangulated")
        else if (f.getVerticesCount == 3) {
          var subTIndex = 0
          f.getVerticesList.foreach( vertexIndex => {
            gIndices.putInt((triangleIndex*3 + subTIndex) * T_INDEX_SIZE, vertexIndex )
            subTIndex += 1
          })
          triangleIndex +=1
        }
        // silently ignore edges and unconnected vertices
      })
    }
    new ByteBufferMeshConverter(totalVerts,totalTriangles,gVertices,gIndices, aabb, pbModel.getName)
  }
  
  /**
   * returns the first vertex found in the pbModel
   */
  def getFirstVertex(pbModel:Model):Option[Point3d] = {
    val vertexList = pbModel.getVerticesList()
    if (vertexList.size>0 ) {
      val firstPBVertex = vertexList(0)
      Option(new Point3d(firstPBVertex.getX, firstPBVertex.getY, firstPBVertex.getZ))
    } else {
      None
    }
  }
}