#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

bl_info = {
  "name": "Toxicblend - Simple gcode viewer",
  'description': 'Simple implementation of gcode visualization.',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
          
class ToxicBlend_SimpleGcodeViewer(bpy.types.Operator):
  '''Simple gcode viewer'''
  bl_idname = "object.toxicblend_simplegcodeviewer"
  bl_label = "Toxicblend:Simple gcode viewer"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  useMultiThreadingProperty = bpy.props.EnumProperty(
    name="Use experimental mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           #update=mode_update_callback
           default="FALSE"    
          )
          
  simplifyLimitProperty = bpy.props.FloatProperty(name="Simplify Limit", default=0.5, min=0.0001, max=100, description="the maximum allowed 3d deviation (in pixels) from a straight line, if the deviation is larger than this the line will be segmented.")  
  filenameProperty = bpy.props.StringProperty(name="Filename", description="the filename of the gcode file. Usually a .ngc file")
  
  def execute(self, context):
    imp.reload(toxicblend) # needed when reloading toxicblend site-packages, won't be used in a release version
    try:
      with toxicblend.ByteCommunicator("localhost", 9999) as bc: 
        unitSystemProperty = context.scene.unit_settings
        properties = {'useMultiThreading'     : str(self.useMultiThreadingProperty),
                      'simplifyLimit'         : str(self.simplifyLimitProperty),
                      'unitSystem'            : str(unitSystemProperty.system), 
                      'unitScale'             : str(unitSystemProperty.scale_length),
                      'filename'              : self.filenameProperty }
        bc.sendOnlyCommand(self.bl_idname, properties) 
        bc.receiveObjects()
        return {'FINISHED'}
    except toxicblend.ToxicblendException as e:
      self.report({'ERROR'}, e.message)
      return {'CANCELLED'}
  
def register():
  # Check Blender version
  req = [2, 69, 0]
  (a,b,c) = bpy.app.version
  if a < req[0] or (a==req[0] and b<req[1]) or (a==req[0] and b == req[1] and c < req[2]):
    msg = 'Blender too old: %s < %s' % ((a,b,c), tuple(version))
    raise NameError(msg)
 
  bpy.utils.register_class(ToxicBlend_SimpleGcodeViewer)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_SimpleGcodeViewer)

if __name__ == "__main__":
  register()