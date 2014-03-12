#!/usr/bin/python   
import bpy
import toxicblend

# import site; site.getsitepackages()

import imp

bl_info = {
  'name': "Toxicblend - Dragon curve",
  'description': 'Generates a parametric dragon curve.',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
       
class AddDragonCurve(bpy.types.Operator):
  '''Adds a dragon curve from toxiclibs server'''
  bl_idname = "object.toxicblend_add_dragon_curve"
  bl_label = "Toxicblend:Add Dragon Curve"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.

  iterations = bpy.props.IntProperty(name="Iterations", default=4, min=1, max=15)
  edgeLength = bpy.props.FloatProperty(name="edgeLength", default=1, min=0.0001, max=100)
      
  def execute(self, context):
    imp.reload(toxicblend)
    cursor_location = bpy.context.scene.cursor_location.copy()
    with toxicblend.ByteCommunicator("localhost", 9999) as c: 
      properties = {'iterations': str(self.iterations),\
                    'edgeLength': str(self.edgeLength),
                    'cursorPosX'            : str(cursor_location.x),
                    'cursorPosY'            : str(cursor_location.y),
                    'cursorPosZ'            : str(cursor_location.z)}
      c.sendOnlyCommand(self.bl_idname, properties) 
      c.receiveObjects()
      return {'FINISHED'}

def register():
  bpy.utils.register_class(AddDragonCurve)

def unregister():
  bpy.utils.unregister_class(AddDragonCurve)

if __name__ == "__main__":
  register()