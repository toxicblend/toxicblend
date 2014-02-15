package org.toxicblend.typeconverters

import toxi.geom.mesh.Mesh3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.Vec2D
import toxi.geom.mesh.TriangleMesh
import toxi.geom.Matrix4x4
import toxi.geom.Vec3D
import toxi.geom.AABB
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.IndexedSeqLike
import scala.collection.mutable.HashMap
import scala.collection.Map
import org.toxicblend.geometry.ProjectionPlane
import org.toxicblend.geometry.Vec2DZ
import org.toxicblend.protobuf.ToxicBlendProtos.{Model,Face}
import org.toxicblend.operations.boostmedianaxis.InteriorEdges
import org.toxicblend.util.VertexToFaceMap

import scala.collection.JavaConversions._

class Mesh3DConverter protected (protected val vertices:Buffer[ReadonlyVec3D], 
                                 protected val faces:Buffer[ArrayBuffer[Int]], 
                                 protected val bounds:AABB, 
                                 val name:String="") {
  
  val vert2id = new HashMap[ReadonlyVec3D,Int]()
  (0 until vertices.size).foreach(i => vert2id.put(vertices(i),i ))
  
  def this(name:String="mesh3d") = {
    this(new ArrayBuffer[ReadonlyVec3D], new ArrayBuffer[ArrayBuffer[Int]], new AABB, name)  
  }
  
  def getVertices:Seq[ReadonlyVec3D] = vertices
  def getFaces:Seq[Seq[Int]] = faces
  def getBounds = bounds
  
  /**
   * Returns the edges of the faces with only 2 vertices
   * @param scale is needed when converting to mm from meter (for example)
   */
  def getEdgesAsVec2DZ(scale:Float=1f):Map[Int,Vec2DZ] = {
    val mapId2vec2dz = new HashMap[Int,Vec2DZ] 
    
    def getOrAddVec2d(objectId:Int):Vec2DZ = {
      if (mapId2vec2dz.contains(objectId)) {
        mapId2vec2dz.get(objectId).get
      } else {
        val aVec2DZ = new Vec2DZ(vertices(objectId).scale(scale),objectId)
        mapId2vec2dz.put(objectId, aVec2DZ)
        aVec2DZ
      }
    }
    
    val trueEdges = faces.filter(f => f.size < 3)
    //val rv = new ArrayBuffer[Vec2DZ]
    trueEdges.foreach(face => {
      if (face.size ==1) {
        //rv += new Vec2DZ(vertices(face(0)),face(0))
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
   * Adds a unique vertex to the vertices list. If a the vertex is already found the vertex id is returned 
   */
  @inline
  protected def addVertex(v:ReadonlyVec3D):Int = {
    if (vert2id.contains(v)) {
      vert2id(v)
    } else {
      val rv = vertices.size
      vertices += v
      vert2id.put(v,rv)
      rv
    }
  }
  
  /**
   * finds continuous segments of edges as vertices (by int id)
   * returns a tuple containing _1 = ngons (faces with 3 or more vertices)
   *                            _2 = line segments of vertices
   */
  def findContinuousLineSegmentsAsId():(IndexedSeq[IndexedSeq[Int]],IndexedSeq[IndexedSeq[Int]]) = {
    val map = new VertexToFaceMap()
    val lineStrings = new ArrayBuffer[ArrayBuffer[ReadonlyVec3D]]
    (0 until faces.size).foreach(faceId => {
      val vertices = faces(faceId)
      val distincts = vertices.distinct
      if (distincts.size != vertices.size) {
        if (distincts.size>1) {
          println("findLineSegments:: This is terrible wrong, i know. But i just took the unique vertices of a face and added them to the result set.") // TODO: fix it
          println("" + vertices.mkString("{",",","}") + " => " + distincts.mkString("{",",","}"))
          map.add(distincts, faceId)
        }
      } else {
        map.add(vertices, faceId)
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
    val ngons = result._1.map(vertexList => vertexList.map(vertexId => this.vertices(vertexId)))
    val lineSegments = result._2.map(vertexList => vertexList.map(vertexId => this.vertices(vertexId)))
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
  def transformOne(inMatrix:Matrix4x4):Mesh3DConverter = {
    bounds.clearAABB()
    (0 until vertices.size).foreach(i=>{
      val vOld = vertices(i)
      val vNew =  inMatrix.applyToSelf(new Vec3D(vOld))
      bounds.growToContainPoint(vNew)
      vertices(i) = vNew
    })
    this
  }
  
  /**
   * Create a packet buffer model from this Mesh3D.
   * The result will be a list of triangles due to the design of Mesh3D
   */  
  def toPBModel(finalTransformation:Option[Matrix4x4Converter], projectionPlane:Option[ProjectionPlane.ProjectionPlane]) = {
    val modelBuilder = org.toxicblend.protobuf.ToxicBlendProtos.Model.newBuilder()
    val matrix = if (finalTransformation.isDefined) {
       {val m=new Matrix4x4(finalTransformation.get.matrix); m.invert(); Option(m) }
    } else None
    
    (0 until vertices.size).foreach(vertexId => {
      val pbvertex = org.toxicblend.protobuf.ToxicBlendProtos.Vertex.newBuilder()
      val origVertex = vertices(vertexId)
      val vertex = projectionPlane match {
        case Some(ProjectionPlane.YZ_PLANE) => new Vec3D(origVertex.z, origVertex.x, origVertex.y)
        case Some(ProjectionPlane.XZ_PLANE) => new Vec3D(origVertex.x, origVertex.z, origVertex.y)
        case Some(ProjectionPlane.XY_PLANE) => new Vec3D(origVertex.x, origVertex.y, origVertex.z)
        case _ => new Vec3D(origVertex.x, origVertex.y, origVertex.z)
      } 
      if (matrix.isDefined) {
        matrix.get.applyToSelf(vertex)
      }
      pbvertex.setX(vertex.x)
      pbvertex.setY(vertex.y)
      pbvertex.setZ(vertex.z)
      pbvertex.setId(vertexId) 
      modelBuilder.addVertices(pbvertex)
      
      if (finalTransformation.isDefined) modelBuilder.setWorldOrientation(finalTransformation.get.toPBModel)
    }) 
    faces.foreach( face => {
      if (face.size==2 && ( face(0) == face(1))) {
        // TODO: fix the cause of this. Blender will crash if there is an edge between a single vertex (and itself)
        System.err.println("Mesh3DConverter::toPBModel: Not adding edge between identical vertices: %d %d".format(face(0),face(1)))
      } else {
        val pbface = Face.newBuilder()
        face.foreach( vertexId => pbface.addVertices(vertexId))
        modelBuilder.addFaces(pbface)
      }
    })
    modelBuilder.setName(name)
    modelBuilder
  } 
  
  /**
   * Adds an edge between two vertices. 
   */
  def addEdges (v1:ReadonlyVec3D, v2:ReadonlyVec3D) = {
    val v1index = addVertex(v1)
    val v2index = addVertex(v2)
    val newFace = new ArrayBuffer[Int](2) += v1index += v2index
    faces += newFace
    this
  }
  
  /**
   * Adds edges between a list of vertices (line segment). 
   */
  def addMultipleEdges (inVertices:IndexedSeq[ReadonlyVec3D]) = {
    if ( 1 == inVertices.size ) {
      System.err.println("addEdges: One single vertex does not build an edge: Debug me")
    }
    inVertices.sliding(2,1).foreach(edge => {
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
   * Adds one face using specified vertices. 
   */
  def addFace (inVertices:IndexedSeq[ReadonlyVec3D]) = {
    if ( 1 == inVertices.size ) {
      System.err.println("addFace: One single vertex does not build an edge: Debug me")
    }
    val newFace = new ArrayBuffer[Int](inVertices.size)
    inVertices.foreach(v => {
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
   * returns the first vertex found in the pbModel
   */
  protected def getFirstVertex(pbModel:Model):Option[ReadonlyVec3D] = {
    val vertexList = pbModel.getVerticesList()
    if (vertexList.size >0 ) {
      val firstPBVertex = vertexList(0)
      Option(new Vec3D(firstPBVertex.getX, firstPBVertex.getY, firstPBVertex.getZ))
    } else {
      None
    }
  }
  
  /** 
   * Constructs from a packet buffer model, if there is a world transformation it will be used to calculate the 'real' vertices
   * @param pbModel the model we are reading from
   * @param useWorldCoordinares convert coordinates in the pbModel into world coordinates (apply world transformation)
   * @param unitScale 'extra' scaling needed to convert from one unit of measure to another (e.g. meter to millimeter)
   */
  def apply(pbModel:Model, useWorldCoordinares:Boolean, unitScale:Float):Mesh3DConverter = {
    
    val hasWorldTransformation = pbModel.hasWorldOrientation()
    val worldTransformation = if (hasWorldTransformation) Option(Matrix4x4Converter(pbModel.getWorldOrientation())) else None
    val vbuffer = new Array[ReadonlyVec3D](pbModel.getVerticesList().size).toBuffer
    val fbuffer = new ArrayBuffer[ArrayBuffer[Int]](pbModel.getFacesList().size)
    
    val aabb = {
      if (useWorldCoordinares && hasWorldTransformation) {
        val wtransform = worldTransformation.get.matrix
        val firstVertexOpt = getFirstVertex(pbModel)
        val aabb = if (firstVertexOpt.isDefined) {
          new AABB(wtransform.applyToSelf(firstVertexOpt.get.scale(unitScale)),0f)
        } else {
          new AABB // no vertices, aabb will have origin at origo
        }
        pbModel.getVerticesList().foreach( vertex => {
          val transformedVector = wtransform.applyToSelf(new Vec3D(vertex.getX*unitScale, vertex.getY*unitScale, vertex.getZ*unitScale))
          aabb.growToContainPoint(transformedVector)
          vbuffer(vertex.getId()) = transformedVector
        })
        aabb
      } else {
        val firstVertexOpt = getFirstVertex(pbModel)
        val aabb = if (firstVertexOpt.isDefined) {
          new AABB(firstVertexOpt.get.scale(unitScale),0f)
        } else {
          new AABB // no vertices, aabb will have origin at origo
        }
        pbModel.getVerticesList().foreach( vertex => {
          val v = new Vec3D(vertex.getX*unitScale, vertex.getY*unitScale, vertex.getZ*unitScale)
          aabb.growToContainPoint(v)
          vbuffer(vertex.getId()) = v
        })
        aabb
      }
    }
    
    pbModel.getFacesList().foreach( face => {
      val eBuffer = new ArrayBuffer[Int](face.getVerticesList().size())
      face.getVerticesList().foreach( v => eBuffer += v )
      fbuffer += eBuffer
    })
    new Mesh3DConverter(vbuffer, fbuffer, aabb, pbModel.getName)
  }
  
  /** 
   * Build a segmented line from a sequence of vertices, each segment will have it's own face structure
   */
  def apply(vertices:Seq[ReadonlyVec3D], name:String) = {
    val aabb = {
      // if possible, use the first vertex when creating the AABB
      if (vertices.size >0 ) {
        val firstVertex = vertices(0)
        new AABB(new Vec3D(firstVertex.x, firstVertex.y, firstVertex.z), 0f)
      } else
        new AABB
    }
    val vbuffer = new ArrayBuffer[ReadonlyVec3D](vertices.size)
    val fbuffer = new ArrayBuffer[ArrayBuffer[Int]](vertices.size+1)
    vertices.foreach( v=> {
      vbuffer+=new Vec3D(v) 
      aabb.growToContainPoint(v)
    })
    (0 until vertices.size).sliding(2,1).foreach( v => {
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
   * remove 3Dvertices that are 'stacked' on the same XY coordinate.  
   */
  def removeZDoubles(interiorEdges:Mesh3DConverter):Mesh3DConverter = {
    val undoubled = new Mesh3DConverter(interiorEdges.name)
    // group every Vec3D that occupy the same Vec2D coordinate into arrays
    // map1={ReadonlyVec2D => [ReadonlyVec3D..]}
    val map1 = new HashMap[ReadonlyVec2D, Array[ReadonlyVec3D]]
    interiorEdges.vertices.foreach (p => {
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
      val v1 = interiorEdges.vertices(edge(0))
      val v2 = interiorEdges.vertices(edge(1))
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
}