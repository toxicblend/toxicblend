package org.toxicblend.operations.volumetricmesh

import org.toxicblend.util.Regex
import org.toxicblend.util.Time.time
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.mesh.LaplacianSmooth
import toxi.geom.AABB
import toxi.geom.Matrix4x4
import toxi.volume.{MeshLatticeBuilder,VolumetricSpace, VolumetricBrush, RoundBrush, BoxBrush, HashIsoSurface}
import toxi.util.datatypes.FloatRange
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.typeconverters.LineStripConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.geometry.Matrix4x4Implicit._

import scala.collection.JavaConversions._

/**
 * This code has evolved out of toxiclibs demonstration code written by Karsten Schmidt
 * http://toxiclibs.org/2011/12/metworks-workshop-facade 
 * Released under the same license as toxiclibs itself, LGPLv2.1.
 */
class VolumetricMeshOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message, options:OptionConverter) = {
    
    // we are only using the first model as input
    val inModel = inMessage.getModelsList.get(0) 
    val traceMsg = "VolumetricMeshOperation"
      
    val voxelBrushSize = options.getFloatProperty("voxelBrushSize", 2f, traceMsg)
    val voxelResolution = options.getFloatProperty("voxelResolution", 128f, traceMsg)    
    val voxelIsoValue = options.getFloatProperty("voxelIsoValue", 0.66f, traceMsg)
    val voxelBrushDrawStep = options.getFloatProperty("voxelBrushDrawStep", 1f, traceMsg)
    val laplacianSmoothIterations = options.getIntProperty("laplacianIterations", 0, traceMsg)
    val runMeshFaceOutwards = options.getBooleanProperty("runMeshFaceOutwards", false, traceMsg)

    // pure magic
    val magicalScalingFactor=1.1f
    
    // create empty container for iso surface mesh
    val mesh = new WETriangleMesh
    val lineStripConverter = time("Converting input to linestrips: ", LineStripConverter(inModel,true))
    val bounds3D = lineStripConverter.bounds.copy
    
    {
      // enlarge the aabb a bit so that the voxels will fit
      val margin = new Vec3D(voxelBrushSize*3f, voxelBrushSize*3f, voxelBrushSize*3f)
      bounds3D.growToContainPoint(bounds3D.getMax.add(margin))
      bounds3D.growToContainPoint(bounds3D.getMin.sub(margin))
    }
    // get the extent of the 3d bounding box enclosing
    // all displaced facade points
    val extent = bounds3D.getExtent
    val builder = {
      // figure out which axis is the longest/largest
      val maxAxis = List(extent.x, extent.y, extent.z).max
      // scale voxel resolution per axis in relation to major axis
      val resX = (extent.x / maxAxis * voxelResolution).toInt
      val resY = (extent.y / maxAxis * voxelResolution).toInt
      val resZ = (extent.z / maxAxis * voxelResolution).toInt
      // create a new mesh lattice builder utility configured
      // to match the current physical size of the facade and voxel resolution
       new MeshLatticeBuilder(extent.scale(2), resX, resY, resZ, new FloatRange(1, 1))
    }
    // use a slightly enlarged bounding box as range for input coordinates
    // it needs to be slightly larger to avoid clipping/thinning of the
    // voxel structure
    // at the sides of the volume
    builder.setInputBounds(new AABB(bounds3D, extent.scale(magicalScalingFactor)))
    // ask the builder for the underlying volumetric/voxel space data
    // structure
    val volume = builder.getVolume
    
    // create a volumetric brush associated with this volume
    val brush = options.getOrElse("voxelBrushType", "SPHERE") match {
      case "SPHERE" =>  new RoundBrush(volume, voxelBrushSize);
      case "BOX" =>  new BoxBrush(volume, voxelBrushSize);      
      case _ => new RoundBrush(volume, voxelBrushSize);
    }
    brush.setMode( options.getOrElse("voxelBrushMode", "MODE_PEAK") match {
      case "MODE_ADDITIVE" => VolumetricBrush.MODE_ADDITIVE
      case "MODE_MULTIPLY" => VolumetricBrush.MODE_MULTIPLY
      case "MODE_REPLACE" => VolumetricBrush.MODE_REPLACE
      // set the brush mode so that lower density values don't overwrite
      // existing higher ones
      case "MODE_PEAK" => VolumetricBrush.MODE_PEAK
      case _ => VolumetricBrush.MODE_PEAK
    })
   
    // now iterate over all shapes and segments within each shape
    for (shape <- lineStripConverter.lineStrips) yield {
      for (segment <- shape.getSegments ) {
        // use the builder class to represent the current line segment
        // as voxels by sweeping the brush along the line at the given
        // step distance (1 unit)
        builder.createLattice(brush, segment, voxelBrushDrawStep);
      }
    }
    // finally ensure the volume will be water tight
    volume.closeSides
    // create an iso surface for the volume and threshold value
    // and turn it into a triangle mesh
    time("Running computeSurfaceMesh: ", new HashIsoSurface(volume).computeSurfaceMesh(mesh, voxelIsoValue));
    
    {
      // laplacian smooth sometimes deforms scale and origin, so i transform the mesh 
      // before smoothing
      val scale:ReadonlyVec3D = {
        val maxAxisInput = {
          val e = lineStripConverter.bounds.getExtent
          List(e.x,e.y,e.z).max+(voxelBrushSize*0.5f)
        }
        val maxAxisOutput = {
          val e = mesh.getBoundingBox.getExtent
          List(e.x,e.y,e.z).max
        }
        val scale = maxAxisInput/maxAxisOutput
        new Vec3D(scale,scale,scale)
      }
      val translate = lineStripConverter.bounds
      //println("Scalaing mesh with " + scale + " and translating to " + translate)
      val m = (new Matrix4x4).translateScale(translate, scale)
      mesh.transform(m)
    }
    
    // apply iterations of the laplacian smooth filter to average
    // neighboring mesh vertices and so reduce voxel aliasing
    
    if (laplacianSmoothIterations > 0) {
      time("Running laplacianSmoothIterations: ", {
        try new LaplacianSmooth().filter(mesh, laplacianSmoothIterations)
	      catch {
	        // sometimes it just throws an exception
	        case e: NullPointerException => {
	          System.out.println("LaplacianSmooth Ignoring exception:")
	          e.printStackTrace
	        }
	      }
      })
    }
    
    if (runMeshFaceOutwards) time("Fixing face normals ", mesh.faceOutwards)
    
    time("Building packet buffer reply: ", {
      val messageBuilder = Message.newBuilder
    
	    val pbModel = Mesh3DConverter(mesh).toPBModel(None, None)
	    pbModel.setName(inModel.getName + " Iso surface")
	    messageBuilder.addModels(pbModel)
    })
  }  
}