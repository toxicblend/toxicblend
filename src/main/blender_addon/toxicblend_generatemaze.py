#!/usr/bin/python   
import bpy
import toxicblend
import imp

bl_info = {
  "name": "Generate maze",
  "category": "Object",
}
       
class ToxicBlend_GenerateMaze(bpy.types.Operator):
  '''Generate maze from a set of edges'''
  bl_idname = "object.toxicblend_generatemaze"
  bl_label = "ToxicBlend Generate Maze"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
    
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend)
    with toxicblend.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      unitSystemProperty = context.scene.unit_settings
      activeObject = context.scene.objects.active
      properties = {'unitSystem'            : str(unitSystemProperty.system), 
                    'unitScale'             : str(unitSystemProperty.scale_length)}
                     
      c.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
      c.receiveObjects()
      return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_GenerateMaze)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_GenerateMaze)

if __name__ == "__main__":
  register()
