#!/usr/bin/python   
import bpy
import toxicblend

# import site; site.getsitepackages()

import imp

bl_info = {
  "name": "Naive implementation of projection outline (toxiclibs service)",
  "category": "Object",
}
       
class ToxicBlend_BoostSimplify(bpy.types.Operator):
  '''Simplify edges using boost'''
  bl_idname = "object.toxicblend_boostsimplify"
  bl_label = "Boost simplify"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  useMultiThreadingProperty = bpy.props.EnumProperty(
    name="Use experimental mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           #update=mode_update_callback
           default="FALSE"    
          )
          
  simplifyLimitProperty = bpy.props.FloatProperty(name="Simplify Limit", default=0.5, min=0.0001, max=100, description="the maximum allowed 3d deviation (in pixels) from a straight line, if the deviation is larger than this the line will be segmented.")  
  
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblender)
    with toxicblender.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      activeObject = context.scene.objects.active
      properties = {'useMultiThreading'     : str(self.useMultiThreadingProperty),
                    'simplifyLimit'         : str(self.simplifyLimitProperty) }
                     
      c.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
      c.receiveObjects()
      return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_BoostSimplify)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_BoostSimplify)

if __name__ == "__main__":
  register()