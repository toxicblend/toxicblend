#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

bl_info = {
  "name": "Naive implementation gcode generation",
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  #'api': 35774,
  'description': 'Converts a set of edges into GCode using toxicblend.',
  "category": "Object",
}
          
class ToxicBlend_SimpleGcodeGenerator(bpy.types.Operator):
  '''Simple gcode generator'''
  bl_idname = "object.toxicblend_simplegcodegenerator"
  bl_label = "ToxicBlend Simple Gcode generator"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  useMultiThreadingProperty = bpy.props.EnumProperty(
    name="Use experimental mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           #update=mode_update_callback
           default="FALSE"    
          )
          
  simplifyLimitProperty = bpy.props.FloatProperty(name="Simplify Limit", default=0.5, min=0.0001, max=100, description="the maximum allowed 3d deviation (in pixels) from a straight line, if the deviation is larger than this the line will be segmented.")  
  filenameProperty = bpy.props.StringProperty(name="Filename", default="gcode.ngc", description="the filename of the gcode file. Usually a .ngc file")
  
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend) # needed when reloading toxicblend site-packages, won't be used in a release version
    with toxicblend.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      activeObject = context.scene.objects.active
      unitSystemProperty = context.scene.unit_settings
      
      properties = {'useMultiThreading'     : str(self.useMultiThreadingProperty),
                    'simplifyLimit'         : str(self.simplifyLimitProperty),
                    'unitSystem'            : str(unitSystemProperty.system), 
                    'unitScale'             : str(unitSystemProperty.scale_length),
                    'filename'              : self.filenameProperty }
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
 
  bpy.utils.register_class(ToxicBlend_SimpleGcodeGenerator)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_SimpleGcodeGenerator)

if __name__ == "__main__":
  register()
