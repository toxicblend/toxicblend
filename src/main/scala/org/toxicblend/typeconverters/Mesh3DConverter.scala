package org.toxicblend.typeconverters

import toxi.geom.mesh.Mesh3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Vec2D
import toxi.geom.mesh.TriangleMesh
import toxi.geom.Matrix4f
import toxi.geom.Vec3D
import toxi.geom.AABB
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.IndexedSeqLike
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.toxicblend.geometry.ProjectionPlane
import org.toxicblend.geometry.Vec2DZ
import org.toxicblend.protobuf.ToxicBlenderProtos.{Model,Face}
import org.toxicblend.operations.boostmedianaxis.InteriorEdges
import org.toxicblend.util.VertexToFaceMap

import scala.collection.JavaConversions._

class Mesh3DConverter protected (protected val vertexes:Buffer[ReadonlyVec3D], protected val faces:Buffer[ArrayBuffer[Int]], 
                                 protected val bounds:AABB, val name:String="") {
  
  val vert2id = new HashMap[ReadonlyVec3D,Int]()
  (0 until vertexes.size).foreach(i => vert2id.put(vertexes(i),i ))
  
  def this(name:String="mesh3d") = {
    this(new ArrayBuffer[ReadonlyVec3D], new ArrayBuffer[ArrayBuffer[Int]], new AABB, name)  
  }
  
  def getVertexes:Seq[ReadonlyVec3D] = vertexes
  def getFaces:Seq[Seq[Int]] = faces
  def getBounds = bounds
  
  /**
   * Returns the edges of the faces with only 2 vertexes
   * @param scale is needed when converting to mm from meter (for example)
   */
  def getEdgesAsVec2DZ(scale:Float=1f):Map[Int,Vec2DZ] = {
    val mapId2vec2dz = new HashMap[Int,Vec2DZ] 
    
    def getOrAddVec2d(objectId:Int):Vec2DZ = {
      if (mapId2vec2dz.contains(objectId)) {
        mapId2vec2dz.get(objectId).get
      } else {
        val aVec2DZ = new Vec2DZ(vertexes(objectId).scale(scale),objectId)
        mapId2vec2dz.put(objectId, aVec2DZ)
        aVec2DZ
      }
    }
    
    val trueEdges = faces.filter(f => f.size < 3)
    //val rv = new ArrayBuffer[Vec2DZ]
    trueEdges.foreach(face => {
      if (face.size ==1) {
        //rv += new Vec2DZ(vertexes(face(0)),face(0))
        getOrAddVec2d(face(0))
      } else if (face.size ==2) { // must be size == 2
        val v1 = getOrAddVec2d(face(0))
        val v2 = getOrAddVec2d(face(1))
        v1.addEdge(v2)
        v2.addEdge(v1)
        //rv += v1
        //rv += v2
      } 
      assert(face.size<=2)
    })
    mapId2vec2dz
  }
  
  /**
   * Adds a unique vertex to the vertexes list. If a the vertex is already found the vertex id is returned 
   */
  @inline
  protected def addVertex(v:ReadonlyVec3D):Int = {
    if (vert2id.contains(v)) {
      vert2id(v)
    } else {
      val rv = vertexes.size
      vertexes += v
      vert2id.put(v,rv)
      rv
    }
  }
  
  /**
   * finds continuous segments of edges as vertexes (by int id)
   * returns a tuple containing _1 = ngons (faces with 3 or more vertices)
   *                            _2 = line segments of vertices
   */
  def findContinuousLineSegmentsAsId():(IndexedSeq[IndexedSeq[Int]],IndexedSeq[IndexedSeq[Int]]) = {
    val map = new VertexToFaceMap()
    val lineStrings = new ArrayBuffer[ArrayBuffer[ReadonlyVec3D]]
    (0 until faces.size).foreach(faceId => {
      val vertexes = faces(faceId)
      val distincts = vertexes.distinct
      if (distincts.size != vertexes.size) {
        if (distincts.size>1) {
          println("findLineSegments:: This is terrible wrong, i know. But i just took the unique vertexes of a face and added them to the result set.") // TODO: fix it
          println("" + vertexes.mkString("{",",","}") + " => " + distincts.mkString("{",",","}"))
          map.add(distincts, faceId)
        }
      } else {
        map.add(vertexes, faceId)
      }
    })
    val result = map.findVertexIdLineStrips
    val ngons = result._1.map(vertexList => vertexList.map(vertexId => vertexId.toInt))
    val lineSegments = result._2.map(vertexList => vertexList.map(vertexId => vertexId.toInt))
    (ngons,lineSegments)
  }
  
  /**
   * finds continuous segments of edges as Vec2DZ
   * returns a tuple containing _1 = ngons (faces with 3 or more vertices)
   *                            _2 = line segments of vertices
   * @param scale is needed when converting to mm from meter (for example)
   */
  def findContinuousLineSegmentsAsVec2DZ(scale:Float=1f):(IndexedSeq[IndexedSeq[Vec2DZ]],IndexedSeq[IndexedSeq[Vec2DZ]]) = {
    val vec2dzmap = getEdgesAsVec2DZ(scale)
    val result = findContinuousLineSegmentsAsId
    val ngons = result._1.map(vertexList => vertexList.map(vertexId => vec2dzmap(vertexId)))
    val lineSegments = result._2.map(vertexList => vertexList.map(vertexId => vec2dzmap(vertexId)))
    (ngons,lineSegments)
  }
  
  /**
   * finds continuous segments of edges. 
   * returns a tuple containing _1 = ngons (faces with 3 or more vertices)
   *                            _2 = line segments of vertices
   */
  def findContinuousLineSegments():(IndexedSeq[IndexedSeq[ReadonlyVec3D]],IndexedSeq[IndexedSeq[ReadonlyVec3D]]) = {
    val result = findContinuousLineSegmentsAsId
    val ngons = result._1.map(vertexList => vertexList.map(vertexId => this.vertexes(vertexId)))
    val lineSegments = result._2.map(vertexList => vertexList.map(vertexId => this.vertexes(vertexId)))
    (ngons,lineSegments)
  }
  
  /**
   * Transforms all the point parameters with this Matrix4f transformation matrix.
   * The fourth element of the point parameters is
   * assumed to be one.
   * 
   * @param matrix the transformation matrix
   * @return this (not a copy)
   */  
  def transformOne(inMatrix:Matrix4f):Mesh3DConverter = {
    bounds.clearAABB()
    (0 until vertexes.size).foreach(i=>{
      val vOld = vertexes(i)
      val vNew =  inMatrix.transformOne(new Vec3D(vOld))
      bounds.growToContainPoint(vNew)
      vertexes(i) = vNew
    })
    this
  }
  
  /**
   * Create a packet buffer model from this Mesh3D.
   * The result will be a list of triangles due to the design of Mesh3D
   */  
  def toPBModel(finalTransformation:Option[Matrix4fConverter], projectionPlane:Option[ProjectionPlane.ProjectionPlane]) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlenderProtos.Model.newBuilder()
    val matrix = if (finalTransformation.isDefined) {
       {val m=new Matrix4f(finalTransformation.get.matrix); m.invert(); Option(m) }
    } else None
    
    (0 until vertexes.size).foreach(vertexId => {
      val pbvertex = org.toxicblend.protobuf.ToxicBlenderProtos.Vertex.newBuilder()
      val origVertex = vertexes(vertexId)
      val vertex = projectionPlane match {
        case Some(ProjectionPlane.YZ_PLANE) => new Vec3D(origVertex.z, origVertex.x, origVertex.y)
        case Some(ProjectionPlane.XZ_PLANE) => new Vec3D(origVertex.x, origVertex.z, origVertex.y)
        case Some(ProjectionPlane.XY_PLANE) => new Vec3D(origVertex.x, origVertex.y, origVertex.z)
        case _ => new Vec3D(origVertex.x, origVertex.y, origVertex.z)
      } 
      if (matrix.isDefined) {
        matrix.get.transformOne(vertex)
      }
      pbvertex.setX(vertex.x)
      pbvertex.setY(vertex.y)
      pbvertex.setZ(vertex.z)
      pbvertex.setId(vertexId) 
      modelBuilder.addVertexes(pbvertex)
      
      if (finalTransformation.isDefined) modelBuilder.setWorldOrientation(finalTransformation.get.toPBModel)
    }) 
    faces.foreach( face => {
      val pbface = Face.newBuilder()
      face.foreach( vertexId => {
        pbface.addVertexes(vertexId)
      })
      modelBuilder.addFaces(pbface)
    })
    modelBuilder.setName(name)
    modelBuilder
  } 
  
  /**
   * Adds an edge between two vertexes. 
   */
  def addEdges (v1:ReadonlyVec3D, v2:ReadonlyVec3D) = {
    val v1index = addVertex(v1)
    val v2index = addVertex(v2)
    val newFace = new ArrayBuffer[Int](2) += v1index += v2index
    faces += newFace
    this
  }
  
  /**
   * Adds edges between a list of vertexes (line segment). 
   */
  def addMultipleEdges (inVertexes:IndexedSeq[ReadonlyVec3D]) = {
    if ( 1 == inVertexes.size ) {
      System.err.println("addEdges: One single vertex does not build an edge: Debug me")
    }
    inVertexes.sliding(2,1).foreach(edge => {
      val v1 = edge(0)
      val v1index = addVertex(v1)
      val v2 = edge(1)
      val v2index = addVertex(v2)
 
      val newFace = new ArrayBuffer[Int](2) += v1index += v2index
      faces += newFace
    })
    this
  }
  
  /**
   * Adds one face using specified vertexes. 
   */
  def addFace (inVertexes:IndexedSeq[ReadonlyVec3D]) = {
    if ( 1 == inVertexes.size ) {
      System.err.println("addFace: One single vertex does not build an edge: Debug me")
    }
    val newFace = new ArrayBuffer[Int](inVertexes.size)
    inVertexes.foreach(v => {
      val vIndex = addVertex(v)
      newFace += vIndex
    })
    faces += newFace
    this
  }
}

object Mesh3DConverter {
  /** 
   * Constructs from a packet buffer model, assuming no world transformation and unitScale = 1.0
   */
  def apply(pbModel:Model):Mesh3DConverter = {
    apply(pbModel,false,1.0f)
  }
  
  /** 
   * Constructs from a packet buffer model. Will convert to world coordinates. Assuming unitScale = 1.0
   */
  def apply(pbModel:Model,useWorldCoordinares:Boolean):Mesh3DConverter = {
    apply(pbModel,useWorldCoordinares,1.0f)
  }
  
  /** 
   * Constructs from a packet buffer model, if there is a world transformation it will be used to calculate the 'real' vertexes
   * @param pbModel the model we are reading from
   * @param useWorldCoordinares convert coordinates in the pbModel into world coordinates (apply world transformation)
   * @param unitScale 'extra' scaling needed to convert from one unit of measure to another (e.g. meter to millimeter)
   */
  def apply(pbModel:Model, useWorldCoordinares:Boolean, unitScale:Float):Mesh3DConverter = {
    val aabb = new AABB
    val vbuffer = new Array[ReadonlyVec3D](pbModel.getVertexesList().size).toBuffer
    val fbuffer = new ArrayBuffer[ArrayBuffer[Int]](pbModel.getFacesList().size)
    val hasWorldTransformation = pbModel.hasWorldOrientation()
    val worldTransformation = if (hasWorldTransformation) Option(Matrix4fConverter(pbModel.getWorldOrientation())) else None
    
    if (useWorldCoordinares && hasWorldTransformation) {
      val wtransform = worldTransformation.get.matrix
      pbModel.getVertexesList().foreach( vertex => {
        val transformedVector = wtransform.transformOne(new Vec3D(vertex.getX*unitScale, vertex.getY*unitScale, vertex.getZ*unitScale))
        aabb.growToContainPoint(transformedVector)
        vbuffer(vertex.getId()) = transformedVector
      })
    } else {
      pbModel.getVertexesList().foreach( vertex => {
        val v = new Vec3D(vertex.getX*unitScale, vertex.getY*unitScale, vertex.getZ*unitScale)
        aabb.growToContainPoint(v)
        vbuffer(vertex.getId()) = v
      })
    }
    
    pbModel.getFacesList().foreach( face => {
      val eBuffer = new ArrayBuffer[Int](face.getVertexesList().size())
      face.getVertexesList().foreach( v => eBuffer += v )
      fbuffer += eBuffer
    })
    new Mesh3DConverter(vbuffer, fbuffer, aabb, pbModel.getName)
  }
  
  /** 
   * Build a segmented line from a sequence of vertexes, each segment will have it's own face structure
   */
  def apply(vertexes:Seq[ReadonlyVec3D], name:String) = {
    val aabb = new AABB
    val vbuffer = new ArrayBuffer[ReadonlyVec3D](vertexes.size)
    val fbuffer = new ArrayBuffer[ArrayBuffer[Int]](vertexes.size+1)
    vertexes.foreach( v=> {
      vbuffer+=new Vec3D(v) 
      aabb.growToContainPoint(v)
    })
    (0 until vertexes.size).sliding(2,1).foreach( v => {
      val edge = new ArrayBuffer[Int](2)
      edge += v(0)
      edge += v(1)
      fbuffer += edge
    })
    new Mesh3DConverter(vbuffer, fbuffer, aabb, name)
  }
  
  /** 
   * Constructs from one toxi.geom.mesh.Mesh3D
   */
  def apply(toxiMesh:Mesh3D, name:String=""):Mesh3DConverter = {
    val vbuffer = new ArrayBuffer[ReadonlyVec3D](toxiMesh.getNumVertices())
    val vmap = new HashMap[ReadonlyVec3D, Int]()
    val fbuffer = new ArrayBuffer[ArrayBuffer[Int]](toxiMesh.getNumFaces())
     
    toxiMesh.getVertices().foreach( v => {
      vmap.put(v, vbuffer.size)
      vbuffer += new Vec3D(v) 
    })
    
    toxiMesh.getFaces().foreach( f => {
      val tmpface = new ArrayBuffer[Int](3)
      tmpface +=  vmap(f.a)
      tmpface +=  vmap(f.b)
      tmpface +=  vmap(f.c)
      fbuffer += tmpface
    })
    new Mesh3DConverter(vbuffer, fbuffer, toxiMesh.getBoundingBox(), name)
  }
  
  /**
   * removes 3Dvertices that are 'stacked' on the same XY coordinate.  
   */
  def removeZDoubles(interiorEdges:Mesh3DConverter):Mesh3DConverter = {
    val undoubled = new Mesh3DConverter(interiorEdges.name)
    // group every Vec3D that occupy the same Vec2D coordinate into arrays
    // map1={ReadonlyVec2D => [ReadonlyVec3D..]}
    val map1 = new HashMap[ReadonlyVec2D, Array[ReadonlyVec3D]]
    interiorEdges.vertexes.foreach (p => {
      val p2d = new Vec2D(p.x, p.y)
      if (map1.contains(p2d)) {
        val tmp = map1(p2d).to[ArrayBuffer] += p
        map1.put(p2d,tmp.toArray)
      } else {
        map1.put(p2d, Array(p) )
      }
      //aabb.growToContainPoint(p)
    })
    // create a map2={ReadonlyVec2D => ReadonlyVec3D} (average value of the previous array)
    val map2 = map1.map(a => (a._1,{ 
        val medium = new Vec3D()
        a._2.foreach(v => medium.addSelf(v))
        medium.scaleSelf(1f/a._2.size)
        //println("averaged " + a._2.size + "points" )
        medium
      })
    )
    map1.clear
    interiorEdges.faces.foreach(segment => segment.sliding(2,1).foreach( edge => {
      val v1 = interiorEdges.vertexes(edge(0))
      val v2 = interiorEdges.vertexes(edge(1))
      val v1as2D = new Vec2D(v1.x, v1.y)
      val v2as2D = new Vec2D(v2.x, v2.y)
      val v1as3Dmedian = map2(v1as2D)
      val v2as3Dmedian = map2(v2as2D)
      undoubled.addEdges(v1as3Dmedian, v2as3Dmedian)
    }))
    val segments = undoubled.findContinuousLineSegments._2
    val rv = new Mesh3DConverter(undoubled.name)
    segments.foreach(segment => rv.addMultipleEdges(segment))
    rv
  }
    
  /*
   * Build from a set of edges
   * /
  def applyIsThisUsed(edges:IndexedSeq[IndexedSeq[ReadonlyVec3D]], objectName:String ):Mesh3DConverter = {
    //val interiorEdges = new InteriorEdges(edges)
    new Mesh3DConverter(objectName)
  }*/
}