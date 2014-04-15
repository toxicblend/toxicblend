#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

bl_info = {
  "name": "Toxicblend - Add Custom Circle",
  'description': 'An example on how parameteric geometry can be created in code',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
       
class ToxicBlend_ParametricCircle(bpy.types.Operator):
  '''Add Custom Circle operation'''
  bl_idname = "object.toxicblend_addparametriccircleoperation"
  bl_label = "Toxicblend:Add Custom Circle"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  drawTypeProperty = bpy.props.EnumProperty(
    name="Draw type",
    description = "Select draw type.",
    items=(("CIRCLE", "CIRCLE",""),
           ("BOX", "BOX",""), 
           ("SPHERE", "SPHERE","")),
           default="CIRCLE"    
          )
            
  useMultiThreadingProperty = bpy.props.EnumProperty(
    description="Each continous ring segment will be processed in a separate thread",
    name="Use mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           default="FALSE"    
          )
          
  simplifyLimitProperty = bpy.props.FloatProperty(name="Simplify Limit (Not used yet)", default=0.5, min=0.0001, max=100, description="the maximum allowed 3d deviation (in pixels) from a straight line, if the deviation is larger than this the line will be segmented.")       
  zEpsilonProperty = bpy.props.FloatProperty(name="z Epsilon", description="Z values smaller than this is considered to be zero, these points enables 'dot product limit'", default=1.5, min=0.00001, max=10)
  interationsProperty = bpy.props.IntProperty(name="Number of iterations", default=10, min=1, max=10)
  
  def execute(self, context):
    imp.reload(toxicblend)
    try:
      with toxicblend.ByteCommunicator("localhost", 9999) as bc:
        unitSystemProperty = context.scene.unit_settings
        activeObject = context.scene.objects.active
      
        cursor_location = bpy.context.scene.cursor_location.copy()
        properties = {'drawTypeProperty'      : str(self.drawTypeProperty), 
                      'useMultiThreading'     : str(self.useMultiThreadingProperty),
                      'simplifyLimit'         : str(self.simplifyLimitProperty),
                      'zEpsilon'              : str(self.zEpsilonProperty),
                      'iterations'            : str(self.interationsProperty),
                      'unitSystem'            : str(unitSystemProperty.system), 
                      'unitScale'             : str(unitSystemProperty.scale_length),
                      'cursorPosX'            : str(cursor_location.x),
                      'cursorPosY'            : str(cursor_location.y),
                      'cursorPosZ'            : str(cursor_location.z)}
                     
        bc.sendOnlyCommand(self.bl_idname, properties) 
        bc.receiveObjects(setOriginToCursor=True)
        return {'FINISHED'}
    except toxicblend.ToxicblendException as e:
      self.report({'ERROR'}, e.message)
      return {'CANCELLED'}
  
def register():
  bpy.utils.register_class(ToxicBlend_ParametricCircle)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_ParametricCircle)

if __name__ == "__main__":
  register()
  