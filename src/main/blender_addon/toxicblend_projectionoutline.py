#!/usr/bin/python   
import bpy
import toxicblend

# import site; site.getsitepackages()

import imp

bl_info = {
  "name": "Naive implementation of projection outline (toxiclibs service)",
  "category": "Object",
}
       
class NaiveConvexHull(bpy.types.Operator):
  '''Naive implementation of projection outline'''
  bl_idname = "object.toxicblender_projection_outline"
  bl_label = "Projection outline"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  projectionPlaneProperty = bpy.props.EnumProperty(
    name="Projection plane",
    items=(("YZ_PLANE", "YZ",""),
           ("XZ_PLANE", "XZ",""), 
           ("XY_PLANE", "XY","")),
           #update=mode_update_callback
           default="XY_PLANE"    
          )
  multiThreadProperty = bpy.props.EnumProperty(
    name="Use experimental multi threading",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           #update=mode_update_callback
           default="TRUE"    
          )
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblender)
    with toxicblender.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      activeObject = context.scene.objects.active
      properties = {'projectionPlane': str(self.projectionPlaneProperty), 
                     'multiThread' : str(self.multiThreadProperty)}
      c.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
      c.receiveObjects()
      return {'FINISHED'}

def register():
  bpy.utils.register_class(NaiveConvexHull)

def unregister():
  bpy.utils.unregister_class(NaiveConvexHull)

if __name__ == "__main__":
  register()