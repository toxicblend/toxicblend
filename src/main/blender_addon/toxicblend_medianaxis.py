#!/usr/bin/python   
import bpy
import toxicblend

# import site; site.getsitepackages()

import imp

bl_info = {
  "name": "Naive implementation of projection outline (toxiclibs service)",
  "category": "Object",
}
       
class ToxicBlend_MedianAxis(bpy.types.Operator):
  '''Naive implementation of median axis'''
  bl_idname = "object.toxicblend_medianaxis"
  bl_label = "Median axis"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  projectionPlaneProperty = bpy.props.EnumProperty(
    name="Choose 2D plane",
    items=(("YZ_PLANE", "YZ",""),
           ("XZ_PLANE", "XZ",""), 
           ("XY_PLANE", "XY","")),
           #update=mode_update_callback
           default="XY_PLANE"    
          )
  useMultiThreadingProperty = bpy.props.EnumProperty(
    name="Use experimental mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           #update=mode_update_callback
           default="FALSE"    
          )
          
  simplifyLimitProperty = bpy.props.FloatProperty(name="Simplify Limit", default=0.5, min=0.0001, max=100, description="the maximum allowed 3d deviation (in pixels) from a straight line, if the deviation is larger than this the line will be segmented.")  
  zEpsilonProperty = bpy.props.FloatProperty(name="z Epsilon", default=1.5, min=0.00001, max=10)
  dotProductLimitProperty = bpy.props.FloatProperty(name="Dot Product Limit", default=0.95, min=0.0001, max=1)
  calculationResolutionProperty = 46338 # sqrt(Int.MaxValue)-2
  
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblender)
    with toxicblender.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      activeObject = context.scene.objects.active
      properties = {'projectionPlane'       : str(self.projectionPlaneProperty), 
                    'useMultiThreading'     : str(self.useMultiThreadingProperty),
                    'simplifyLimit'         : str(self.simplifyLimitProperty),
                    'zEpsilon'              : str(self.zEpsilonProperty),
                    'dotProductLimit'       : str(self.dotProductLimitProperty),
                    'calculationResolution' : str(self.calculationResolutionProperty) }
                     
      c.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
      c.receiveObjects()
      return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_MedianAxis)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_MedianAxis)

if __name__ == "__main__":
  register()