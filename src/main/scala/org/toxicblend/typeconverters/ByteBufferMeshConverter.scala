package org.toxicblend.typeconverters

import org.toxicblend.protobuf.ToxicBlendProtos.Model
import org.toxicblend.protobuf.ToxicBlendProtos.Face
import javax.vecmath.Vector3d
import javax.vecmath.Point3d
import javax.vecmath.Matrix4d
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.JavaConversions._
import org.toxicblend.ToxicblendException
import com.bulletphysics.linearmath.AABB

class ByteBufferMeshConverter(val totalVerts:Int, val totalTriangles:Int, val gVertices:ByteBuffer, val gIndices:ByteBuffer, val aabb:AABB, val name:String) {
  
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
      var index = 0
      pbModel.getFacesList.foreach(f => {
        if (f.getVerticesCount > 3 ) 
          throw new ToxicblendException("JBullet mesh must be triangulated")
        else if (f.getVerticesCount == 3) {
          var subIndex = 0
          f.getVerticesList.foreach( vertexIndex => {
            gIndices.putInt((index*3 + subIndex) * T_INDEX_SIZE, vertexIndex )
            subIndex += 1
          })
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