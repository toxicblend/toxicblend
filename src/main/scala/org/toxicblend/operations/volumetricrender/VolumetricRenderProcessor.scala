package org.toxicblend.operations.volumetricrender

import org.toxicblend.util.Regex
import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.Vec3D
import toxi.geom.mesh.LaplacianSmooth
import toxi.geom.AABB
import toxi.geom.Matrix4x4
import toxi.volume.{MeshLatticeBuilder,VolumetricSpace, VolumetricBrush, RoundBrush, BoxBrush, HashIsoSurface}
import toxi.util.datatypes.FloatRange
import scala.collection.JavaConversions._
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.typeconverters.LineStripConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.geometry.Matrix4x4Implicit._

/**
 * This code has evolved out of toxiclibs demonstration code written by Karsten Schmidt
 * http://toxiclibs.org/2011/12/metworks-workshop-facade 
 * Released under the same license as toxiclibs itself, LGPLv2.1.
 * 
 * TODO: There are an issue with scaling and centering of the resulting mesh
 */
class VolumetricRenderProcessor extends CommandProcessorTrait {
  
   def processInput(inMessage:Message) = {
    
    // we are only using the first model as input
    val inModel = inMessage.getModelsList().get(0) 
    val options = OptionConverter(inMessage)
    //println(optionM.options)
    val voxelBrushSize:Float = options.getOrElse("voxelBrushSize", "2") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("VolumetricRenderOperation: unrecognizable 'voxelBrushSize' property value: " +  s ); 2f
    }
    val voxelResolution:Float = options.getOrElse("voxelResolution", "128") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("VolumetricRenderOperation: unrecognizable 'voxelResolution' property value: " +  s ); 128f
    }
    val voxelIsoValue:Float = options.getOrElse("voxelIsoValue", "0.66") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("VolumetricRenderOperation: unrecognizable 'voxelIsoValue' property value: " +  s ); 0.66f
    }
    val voxelBrushDrawStep:Float = options.getOrElse("voxelBrushDrawStep", "1.0") match {
      case Regex.FLOAT_REGEX(limit) => limit.toFloat
      case s:String => System.err.println("VolumetricRenderOperation: unrecognizable 'voxelIsoValue' property value: " +  s ); 1f
    }
    val laplacianIterations:Int = options.getOrElse("laplacianIterations", "0") match {
      case Regex.INT_REGEX(limit) => limit.toInt
      case s:String => System.err.println("VolumetricRenderOperation: unrecognizable 'laplacianIterations' property value: " +  s ); 0
    }
    // pure magic
    val magicalScalingFactor=1.1f
    
    // create empty container for iso surface mesh
    val mesh = new WETriangleMesh();
    val lineStripConverter = LineStripConverter(inModel,true)
    val bounds3D = lineStripConverter.bounds.copy;
    
    {
      // enlarge the aabb a bit so that the voxels will fit
      val margin = new Vec3D(voxelBrushSize*3f, voxelBrushSize*3f, voxelBrushSize*3f)
      bounds3D.growToContainPoint(bounds3D.getMax.add(margin))
      bounds3D.growToContainPoint(bounds3D.getMin.sub(margin))
    }
    // get the extent of the 3d bounding box enclosing
    // all displaced facade points
    val extent = bounds3D.getExtent
    // figure out which axis is the longest/largest
    val maxAxis = List(extent.x, extent.y, extent.z).max;
    // scale voxel resolution per axis in relation to major axis
    val resX = (extent.x / maxAxis * voxelResolution).toInt;
    val resY = (extent.y / maxAxis * voxelResolution).toInt;
    val resZ = (extent.z / maxAxis * voxelResolution).toInt;
    // create a new mesh lattice builder utility configured
    // to match the current physical size of the facade and voxel resolution
    val builder = new MeshLatticeBuilder(extent.scale(2), resX, resY, resZ, new FloatRange(1, 1));
    // use a slightly enlarged bounding box as range for input coordinates
    // it needs to be slightly larger to avoid clipping/thinning of the
    // voxel structure
    // at the sides of the volume
    builder.setInputBounds(new AABB(bounds3D, extent.scale(magicalScalingFactor)));
    // ask the builder for the underlying volumetric/voxel space data
    // structure
    val volume = builder.getVolume();
    // create a volumetric brush associated with this volume and using a
    // small brush size
    // VolumetricBrush brush = new BoxBrush(volume, 3.33f);
    
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
    new HashIsoSurface(volume).computeSurfaceMesh(mesh, voxelIsoValue)
    // center the mesh around the world origin (0,0,0)
    //mesh.center(new Vec3D(0, 0, 0));
    // apply 2 iterations of the laplacian smooth filter to average
    // neighboring mesh vertices and so reduce voxel aliasing
    
    if (laplacianIterations > 0) {
      try {
        new LaplacianSmooth().filter(mesh, laplacianIterations)
        println("Done doing "+laplacianIterations + " laplacian smooth iterations")
      } catch {
        // sometimes it just throws an exception
        case e: NullPointerException => e.printStackTrace
      }
    }
    
    {
      // scale back the 1.1 multiplication we did earlier
      // TODO: find out what to scale with here, nothing seems to work
      val scale = new Vec3D(1,1,1)//.scaleSelf(1f/magicalScalingFactor)
      val translate = lineStripConverter.bounds
      val m = (new Matrix4x4).translateScale(translate, scale)
      mesh.transform(m)
    }
    //mesh.scale(1f/magicalScalingFactor)
    //mesh.translate(lineStripConverter.bounds)
    
    val messageBuilder = Message.newBuilder
    val pbModel = Mesh3DConverter(mesh).toPBModel(None, None)
    //if (inModel.hasWorldOrientation()) {
      // simply copy the world orientation
    //  val mConverter = Matrix4x4Converter(inModel.getWorldOrientation)
    //  pbModel.setWorldOrientation(mConverter.toPBModel)
    //}
    pbModel.setName("Iso surface")
    messageBuilder.addModels(pbModel)
    messageBuilder
  }  
}