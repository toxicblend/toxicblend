#!/usr/bin/python   
import bpy
import toxicblend

# import site; site.getsitepackages()

import imp

bl_info = {
  "name": "Creates a dragon curve (toxiclibs service)",
  "category": "Object",
}
       
class AddDragonCurve(bpy.types.Operator):
  '''Adds a dragon curve from toxiclibs server'''
  bl_idname = "object.toxicblender_add_dragon_curve"
  bl_label = "Add Dragon Curve"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.

  iterations = bpy.props.IntProperty(name="Iterations", default=4, min=1, max=15)
  edgeLength = bpy.props.FloatProperty(name="edgeLength", default=1, min=0.0001, max=100)
      
  #@classmethod
  #def poll(cls, context):
  #    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblender)
    with toxicblender.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      properties = {'iterations': str(self.iterations),\
                    'edgeLength': str(self.edgeLength)}
      c.sendOnlyCommand(self.bl_idname, properties) 
      #sendRandomMessage(s)
      c.receiveObjects()
      return {'FINISHED'}

def register():
  bpy.utils.register_class(AddDragonCurve)

def unregister():
  bpy.utils.unregister_class(AddDragonCurve)

if __name__ == "__main__":
  register()