package org.toxicblend.operations.volumetricrender

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.protobuf.ToxicBlendProtos.Model
import toxi.geom.mesh.WETriangleMesh
import toxi.geom.Vec3D
import toxi.geom.AABB
import toxi.volume.{MeshLatticeBuilder,VolumetricSpace, VolumetricBrush, RoundBrush, BoxBrush, HashIsoSurface}
import toxi.util.datatypes.FloatRange
import scala.collection.JavaConversions._
import org.toxicblend.typeconverters.Matrix4x4Converter
import org.toxicblend.typeconverters.LineStripConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.typeconverters.OptionConverter


class VolumetricRenderProcessor extends CommandProcessorTrait {
  
   def processInput(inMessage:Message) = {
    
    // we are only using the first model as input
    val inModel = inMessage.getModelsList().get(0) 
    val options = OptionConverter(inMessage)
    //println(optionM.options)
    val voxelBrushSize = options.getOrElse("voxelBrushSize", "2").toFloat
    val voxelResolution = options.getOrElse("voxelResolution", "128").toFloat
    val voxelIsoValue = options.getOrElse("voxelIsoValue", "0.66").toFloat
    val voxelBrushDrawStep = options.getOrElse("voxelBrushDrawStep", "1.0").toFloat
    
    // create empty container for iso surface mesh
    val mesh = new WETriangleMesh();
    val lineStripConverter = LineStripConverter(inModel)
    val bounds3D = lineStripConverter.bounds.copy()
    val min = bounds3D.getMin();
    val max = bounds3D.getMax();
    
    bounds3D.growToContainPoint(max.add(new Vec3D(voxelBrushSize*3f, voxelBrushSize*3f, voxelBrushSize*3f)));
    bounds3D.growToContainPoint(min.sub(new Vec3D(voxelBrushSize*3f, voxelBrushSize*3f, voxelBrushSize*3f)));
    // get the extent of the 3d bounding box enclosing
    // all displaced facade points
    val extent = bounds3D.getExtent();
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
    builder.setInputBounds(new AABB(bounds3D, extent.scale(1.1f)));
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
      for (segment <- shape.getSegments() ) {
        // use the builder class to represent the current line segment
        // as voxels by sweeping the brush along the line at the given
        // step distance (1 unit)
        builder.createLattice(brush, segment, voxelBrushDrawStep);
      }
    }
    // finally ensure the volume will be water tight
    volume.closeSides();
    // create an iso surface for the volume and threshold value
    // and turn it into a triangle mesh
    (new HashIsoSurface(volume)).computeSurfaceMesh(mesh, voxelIsoValue);
    // center the mesh around the world origin (0,0,0)
    //mesh.center(new Vec3D(0, 0, 0));
    // apply 2 iterations of the laplacian smooth filter to average
    // neighboring mesh vertices and so reduce voxel aliasing
    
    //if (laplacianSmoothIterations > 0) {
    //  try {
    //    new LaplacianSmooth().filter(mesh, laplacianSmoothIterations);
    //  } catch (NullPointerException e) {
    //    e.printStackTrace();
    //  }
    // }
    
    // scale back the 1.1 muliplication we did earlier
    //mesh.scale(1f/1.1f)
    
    val messageBuilder = Message.newBuilder()
    val pbModel = Mesh3DConverter(mesh).toPBModel(None, None) // TODO: replace None with the real world orientation
    if (inModel.hasWorldOrientation()) {
      // simply copy the world orientation
      val mConverter = Matrix4x4Converter(inModel.getWorldOrientation)
      pbModel.setWorldOrientation(mConverter.toPBModel)
    }
    pbModel.setName("Iso surface")
    messageBuilder.addModels(pbModel)
    messageBuilder
  }  
}