package org.toxicblend.operations.boostmedianaxis

import org.toxicblend.util.SharedLibraryLoader
import toxi.geom.{Vec2D=>TVec2D}
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec2D
import toxi.geom.ReadonlyVec3D
import scala.collection.mutable.ArrayBuffer
import org.toxicblend.geometry.Rings2D
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.vecmath.Vec2D

class MedianAxisJni private {
  
  @native protected def allocateJni_():Long // Define allocator
  @native protected def deallocateJni_(instanceId:Long):Unit  // Define de-allocator
  @native protected def getRingJni_(instanceId:Long, ringId:Int):Array[Float] 
  @native protected def setRingJni_(instanceId:Long, ringId:Int, verts:Array[Float], simplifyLimit:Float)
  @native protected def addRingJni_(instanceId:Long, verts:Array[Float], simplifyLimit:Float):Int
  @native protected def ringContainsAllPointsJni_(instanceId:Long, points:Array[Float], ringId:Int):Boolean
  @native protected def ringContainsRingJni_(instanceId:Long, outerRingId:Int, innerRingId:Int):Boolean
  @native protected def voronoiInternalEdgesJni_(instanceId:Long, outerRingIds:Array[Int], internalRingIds:Array[Int], zEpsilon:Float, dotProductLimit:Float, calculationResolution:Float):Array[Float];
  @native protected def simplify3D_(data:Array[Float], limit:Float):Array[Float];
  @native protected def simplify2D_(data:Array[Float], limit:Float):Array[Float];
  
  protected var instanceId = allocateJni_();  // var because the "destructor()" will try to set it to zero
  
  def destructor():Unit = {
    if (instanceId!=0)
    deallocateJni_(instanceId);
    instanceId = 0;
  }
  
  def loadRings2D(rings2D:Rings2D, simplifyLimit:Float, objectName:String):Array[Ring2D] = {
    val foundRings = new Array[Ring2D]( rings2D.rings.size)
    (0 until rings2D.rings.size) foreach( i => {
      val ringVertSequence = rings2D.rings(i).map(x=>rings2D.vertices(x))
      foundRings(i) = new Ring2D(this, objectName, ringVertSequence, new Array[Ring2D](0), simplifyLimit)
    })    
    foundRings
  }
  
  def getRing(ringId:Int):ArrayBuffer[Vec2D] = {
    val rv = MedianAxisJni.fromJni2dArray(getRingJni_(instanceId, ringId))
    if ( rv.size > 1) {
      System.err.println("Debug me, getRingJni_ returned multiple segments")
    }
    rv(0)
  }
  
  def setRing(ringId:Int, inVerts:Seq[Vec2D], simplifyLimit:Float):Unit = {
    val verts = MedianAxisJni.toJni2dArray(inVerts)
    setRingJni_(instanceId, ringId, verts.toArray, simplifyLimit);
  }
  
  def addRing(inVerts:Seq[Vec2D], simplifyLimit:Float):Int = {
    val verts = MedianAxisJni.toJni2dArray(inVerts)
    return addRingJni_(instanceId, verts.toArray, simplifyLimit)
  }
    
  def ringContainsAllPoints(ringId:Int, inPoints:Seq[Vec2D]) : Boolean = {
    val points = MedianAxisJni.toJni2dArray(inPoints)
    return ringContainsAllPointsJni_(instanceId, points.toArray, ringId);
  }
 
  @inline
  def voronoiInternalEdges(ring:Ring2D, zEpsilon:Float, dotProductLimit:Float, calculationResolution:Float, simplifyLimit:Float):Mesh3DConverter = {
     voronoiInternalEdges(ring.outerSegments, ring.innerSegments, zEpsilon, dotProductLimit, calculationResolution, simplifyLimit)
  }
  
  def voronoiInternalEdges(outerSegments:Array[Int], innerSegments:Array[Int], zEpsilon:Float, dotProductLimit:Float, calculationResolution:Float, simplifyLimit:Float):Mesh3DConverter = {
    var tmp = voronoiInternalEdgesJni_(instanceId, outerSegments, innerSegments, zEpsilon, dotProductLimit, calculationResolution)
    System.err.println("Tried to run simplify3D_ on raw voronoiInternalEdgesJni_ output, fix me") // TODO
    if (false && !simplifyLimit.isNaN() && simplifyLimit!=0f){
      tmp = simplify3D_(tmp, simplifyLimit)
    }
    MedianAxisJni.fromJni3dArrayToMesh3D(tmp)
  }
}

object MedianAxisJni {
  
  try {
    SharedLibraryLoader.load("toxicblendboost");
  } catch {
    case e:java.lang.UnsatisfiedLinkError =>
      System.out.println("java.library.path=" + System.getProperty("java.library.path"));
      throw e;
  }
  
  private val mal = new MedianAxisJni
  
  /**
   * By making this apply method the only way to create a MedianAxisJni 
   * instance we ensure that the SharedLibraryLoader code is executed
   */
  def apply():MedianAxisJni = {
    new MedianAxisJni
  }
  
  @inline protected def addToJni3dArray(data:Seq[ReadonlyVec3D], container:ArrayBuffer[Float]):ArrayBuffer[Float]= {
    data.foreach (p => {
      container += p.x
      container += p.y
      container += p.z
    })
    container += Float.NaN
  }
  
  @inline protected def addToJni2dArray(data:Seq[Vec2D], container:ArrayBuffer[Float]):ArrayBuffer[Float]= {
    data.foreach (p => {
      container += p.x.toFloat
      container += p.y.toFloat
    })
    container += Float.NaN
  }
  
  @inline protected def toJni3dArray(data:Seq[ReadonlyVec3D]):ArrayBuffer[Float]= {
    val outBuffer = new ArrayBuffer[Float](data.size/3 + 1)
    data.foreach (p => {
      outBuffer += p.x
      outBuffer += p.y
      outBuffer += p.z
    })
    outBuffer += Float.NaN
    outBuffer
  }
  
  @inline protected def toJni2dArray(data:Seq[Vec2D]):ArrayBuffer[Float]= {
    val outBuffer = new ArrayBuffer[Float](data.size/2 + 1)
    data.foreach (p => {
      outBuffer += p.x.toFloat
      outBuffer += p.y.toFloat
    })
    outBuffer // += Float.NaN
  }
  
  @inline protected def fromJni2dArray(data:Seq[Float]):ArrayBuffer[ArrayBuffer[Vec2D]]= {
    val outBuffer = new ArrayBuffer[ArrayBuffer[Vec2D]]
    var segmentBuffer = new ArrayBuffer[Vec2D]
      var i = 0
      do {
        if (data(i).isNaN())  {
          // New segment found, store and restart the segmentBuffer
          if (segmentBuffer.size > 0) {
            outBuffer += segmentBuffer
            segmentBuffer = new ArrayBuffer[Vec2D]
          }
          i += 1
        } else {
          // Still collecting the same segment, append to segmentBuffer
          if (i+1 < data.size && !data(i+1).isNaN() ) {
            segmentBuffer += Vec2D(data(i), data(i+1))
            i+= 2
          } else {
            System.err.println("MedianAxisJni.fromJni2dArray: debug me plz")
          }
        }
      } while (i < data.size)
        if (segmentBuffer.size > 0) {
          outBuffer += segmentBuffer
        }
     outBuffer
  }
  
  @inline protected def fromJni3dArray(data:Seq[Float]):ArrayBuffer[ArrayBuffer[ReadonlyVec3D]]= {
    val outBuffer = new ArrayBuffer[ArrayBuffer[ReadonlyVec3D]]
    var segmentBuffer = new ArrayBuffer[ReadonlyVec3D]
    var i = 0
    do {
      if (data(i).isNaN())  {
        // New segment found, store and restart the segmentBuffer
        if (segmentBuffer.size > 0) {
          outBuffer += segmentBuffer
          segmentBuffer = new ArrayBuffer[ReadonlyVec3D]
        }
        i += 1
      } else {
        // Still collecting the same segment, append to segmentBuffer
        if (i+2 < data.size && !data(i+1).isNaN() && !data(i+2).isNaN() ) {
          segmentBuffer += new Vec3D(data(i), data(i+1), data(i+2))
          i+= 3
        } else {
          System.err.println("MedianAxisJni.fromJni3dArray: debug me plz")
          // WTF???
        }
      }
    } while (i < data.size)
    if (segmentBuffer.size > 0) {
      outBuffer += segmentBuffer
      //System.err.println("MedianAxisJni.fromJni3dArray: debug me plz")
    }
    outBuffer
  }
  
  @inline protected def fromJni3dArrayToMesh3D(data:Seq[Float]):Mesh3DConverter= {
    val outBuffer = new Mesh3DConverter
    var segmentBuffer = new ArrayBuffer[ReadonlyVec3D]
    var i = 0
    def storeSegmentBuffer = {
      if (segmentBuffer.size > 0) {
        if (1 == segmentBuffer.size ){
          System.err.println("fromJni3dArrayToMesh3D: One single vertex does not make an edge: Debug me. i=" + i)
          System.err.println(segmentBuffer.mkString("{",",","}"))
          //System.err.println(data.mkString("{",",","}"))
        } else {
          segmentBuffer.sliding(2,1).foreach(p2p => outBuffer.addEdges( p2p ))
        }
        segmentBuffer.clear
      }
    }
    while (i < data.size) {
      if (data(i).isNaN())  {
        // New segment found, store and restart the segmentBuffer
        storeSegmentBuffer
        i += 1
      } else {
        // Still collecting the same segment, append to segmentBuffer
        if (i+2 < data.size && !data(i+1).isNaN() && !data(i+2).isNaN() ) {
          segmentBuffer += new Vec3D(data(i), data(i+1), -data(i+2))
          i += 3 
        } else {
          var msg = ""
          if (i>3) msg += ".."+data(i-2)+","+data(i-1)+","+data(i)
          if (i+2<=data.size) msg += ","+data(i+1)+","+data(i+2)+","+data(+3)+".."
          System.err.println(msg+"\nfromJni3dArray:fromJni3dArrayToMesh3D debug me plz")
          System.err.println
          assert(false)
        }
      }
    } 
    storeSegmentBuffer
    outBuffer
  }
   
  def simplify3D(inData:IndexedSeq[ReadonlyVec3D], limit:Float):IndexedSeq[ReadonlyVec3D] = {
    val outBuffer = new ArrayBuffer[Float]

    addToJni3dArray(inData, outBuffer)
    val jniresult = mal.simplify3D_(outBuffer.toArray, limit)
    val outData = fromJni3dArray(jniresult)(0)  // we are only expecting one segment out, pick the first one. TODO make a special method for this
    if (outData.head != inData.head ) {
      System.err.println("simplify3D::0 differs, debug me")
      outData.insert(0,inData.last)
    }
    if (outData.last!=inData.last) {
      System.err.println("simplify3D::last differs, debug me")
      outData += inData.last
    }
    outData
  }
  
  def simplify2D(data:Seq[Seq[Vec2D]], limit:Float):IndexedSeq[IndexedSeq[Vec2D]] = {
    val outBuffer = new ArrayBuffer[Float]
    var i = 0
    data.foreach( d => addToJni2dArray(d, outBuffer))
    fromJni2dArray(mal.simplify2D_(outBuffer.toArray, limit))
  } 
  
  def simplify2DSingle(data:Seq[Vec2D], limit:Float):IndexedSeq[Vec2D] = {
    val outBuffer = new ArrayBuffer[Float]
    addToJni2dArray(data, outBuffer)
    fromJni2dArray(mal.simplify2D_(outBuffer.toArray, limit))(0)
  }
}