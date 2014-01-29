#!/usr/bin/python   
import bpy
import bmesh
import mathutils
import math

bl_info = {
  "name": "Naive implementation of volumetric edge fill",
  "category": "Object",
}
       
class ToxicBlend_MetaVolume(bpy.types.Operator):
  '''Volumetric edge fill using meta capsules'''
  bl_idname = "object.toxicblend_metavolume"
  bl_label = "Metacapsule Volume"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  CAPSULE_VECTOR = mathutils.Vector((1.0, 0.0, 0.0)) # capsule orientation
            
  radiusProperty = bpy.props.FloatProperty(name="Radius", default=1.0, min=0.0001, max=1000, description="Radius of the meta capsules")  
  resolutionProperty = bpy.props.FloatProperty(name="Resolution", default=0.25, min=0.05, max=1, description="Resolution of the meta capsules")  
  thresholdProperty = bpy.props.FloatProperty(name="Threshold", default=0.05, min=0.001, max=1.99999, description="Threshold of the meta capsules")  
  
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def getRotationTo(self, fromVn, toVn):
    """Returns the rotation quaterninon needed when rotating fromVn to toVn. fromVn and toVn should be normalized."""
    if (fromVn[0] == toVn[0] and fromVn[1] == toVn[1] and fromVn[2] == toVn[2] ):
      return mathutils.Quaternion((1.0, 0, 0, 0))
    crossProduct = fromVn.cross(toVn)
    crossProduct.normalize()
    angle = math.acos(fromVn.dot(toVn))
    return mathutils.Quaternion(crossProduct,angle)
 
  def newCapsule(self, metaFactory, v0, v1, radius) : 
    #print("fromV= %s" % str(v0))
    #print("toV= %s" % str(v1))
  
    segment = v1-v0
    #print("segment= %s" % segment)
    capsule = metaFactory.new()
    capsule.co = (v1 + v0) / 2.0
    capsule.type = 'CAPSULE'
    #ele.use_negative = False 
    capsule.radius = radius
    #print("length/2 = %f" % (segment.length / 2.0))
    capsule.size_x = segment.length/2.0
    direction = segment.normalized()
    #print("direction = %s" % direction )
    quaternion = self.getRotationTo(self.CAPSULE_VECTOR,direction)
    capsule.rotation = quaternion
    #print("quaternion = %s" % quaternion )
    return capsule
       
  def execute(self, context):
      
    print("radius %f" %self.radiusProperty)
    
    sourceBm = bmesh.new()
    sourceBm.from_mesh(bpy.context.scene.objects.active.data)
    worldOriention = bpy.context.scene.objects.active.matrix_world.copy()
    mball = bpy.data.metaballs.new("Volumetric metacapsules")
    #print("Resolution = %f" % self.resolutionProperty)
    mball.resolution = self.resolutionProperty
    mball.threshold = self.thresholdProperty
    metaObj = bpy.data.objects.new("Volumetric metacapsules", mball)

    for edge in sourceBm.edges:
      fromV = mathutils.Vector(edge.verts[0].co)
      toV = mathutils.Vector(edge.verts[1].co)
      self.newCapsule(mball.elements, fromV, toV, self.radiusProperty) 
      
    bpy.context.scene.objects.link(metaObj)
    bpy.context.scene.objects.active = metaObj
    metaObj.select = True  

    metaObj.matrix_world = worldOriention
    #metaObj.update(calc_edges=True)
    return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_MetaVolume)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_MetaVolume)

if __name__ == "__main__":
  register()