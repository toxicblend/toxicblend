#!/usr/bin/python   
import bpy
import toxicblend
import imp

# How to install this plugin:
# 
# run this in the blender console:
#   import site; site.getsitepackages()
#
# copy the content of the toxicblend/src/main/blender_addon/site-packages directory to one of the 
# directories listed by the previous command. 
# 
# OSX example:
# cp -R toxicblend/src/main/blender_addon/site-packages/* /Applications/Blender-2.72b/blender-2.72b.app/Contents/MacOS/../Resources/2.72/python/lib/python3.4/site-packages
#
# then restart blender and use "Run script" on this file

bl_info = {
  "name": "Toxicblend - projection outline",
  'description': 'Naive, and really slow, implementation of projection outline',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
       
class NaiveConvexHull(bpy.types.Operator):
  '''Naive implementation of projection outline'''
  bl_idname = "object.toxicblend_projection_outline"
  bl_label = "Toxicblend Projection outline"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  projectionPlaneProperty = bpy.props.EnumProperty(
    name="Projection plane",
    items=(("YZ_PLANE", "YZ",""),
           ("XZ_PLANE", "XZ",""), 
           ("XY_PLANE", "XY","")),
           default="XY_PLANE"    
          )
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend)
    try:
      with toxicblend.ByteCommunicator("localhost", 9999) as bc: 
        activeObject = context.scene.objects.active
        properties = {'projectionPlane': str(self.projectionPlaneProperty)}
        bc.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
        bc.receiveObjects(removeDoublesThreshold=0.0001)
        return {'FINISHED'}
    except toxicblend.ToxicblendException as e:
      self.report({'ERROR'}, e.message)
      return {'CANCELLED'}
  
def register():
  bpy.utils.register_class(NaiveConvexHull)

def unregister():
  bpy.utils.unregister_class(NaiveConvexHull)

if __name__ == "__main__":
  register()