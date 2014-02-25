#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

bl_info = {
  "name": "Toxicblend - SaveObj",
  'description': 'Saves an object in a format native to toxicblend (for testing purposes).',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
          
class ToxicBlend_SaveObj(bpy.types.Operator):
  '''Simple gcode generation'''
  bl_idname = "object.toxicblend_saveobj"
  bl_label = "Toxicblend:Save object (for testing purposes)"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.

  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend) # needed when reloading toxicblend site-packages, won't be used in a release version
    with toxicblend.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      activeObject = context.scene.objects.active
      unitSystemProperty = context.scene.unit_settings
      
      properties = {'unitSystem'            : str(unitSystemProperty.system), 
                    'unitScale'             : str(unitSystemProperty.scale_length) }
      c.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
      c.receiveObjects()
      return {'FINISHED'}

def register():
  # Check Blender version
  req = [2, 69, 0]
  (a,b,c) = bpy.app.version
  if a < req[0] or (a==req[0] and b<req[1]) or (a==req[0] and b == req[1] and c < req[2]):
    msg = 'Blender too old: %s < %s' % ((a,b,c), tuple(version))
    raise NameError(msg)
 
  bpy.utils.register_class(ToxicBlend_SaveObj)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_SaveObj)

if __name__ == "__main__":
  register()