#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

bl_info = {
  "name": "Toxicblend - Boost simplify",
  'description': 'Boost implementation of Douglas-Peucker simplification',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
       
class ToxicBlend_BoostSimplify(bpy.types.Operator):
  '''Simplify edges using boost'''
  bl_idname = "object.toxicblend_boostsimplify"
  bl_label = "Toxicblend:Boost simplify"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  useMultiThreadingProperty = bpy.props.EnumProperty(
    name="Use experimental mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           default="FALSE"    
          )
          
  simplifyLimitProperty = bpy.props.FloatProperty(name="Simplify Limit [mm]", default=0.1, min=0.001, max=100, description="the maximum allowed 3d deviation (in mm) from a straight line, if the deviation is larger than this the line will be segmented.")  
  
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend)
    try:
      with toxicblend.ByteCommunicator("localhost", 9999) as bc: 
        unitSystemProperty = context.scene.unit_settings
        activeObject = context.scene.objects.active
        properties = {'useMultiThreading'     : str(self.useMultiThreadingProperty),
                      'simplifyLimit'         : str(self.simplifyLimitProperty),
                      'unitSystem'            : str(unitSystemProperty.system), 
                      'unitScale'             : str(unitSystemProperty.scale_length) }
                     
        bc.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
        # remove doubles with mm as unit and half the resolution of the simplify operation
        bc.receiveObjects(removeDoublesThreshold=self.simplifyLimitProperty/(unitSystemProperty.scale_length*2000.))
        return {'FINISHED'}
    except toxicblend.ToxicblendException as e:
      self.report({'ERROR'}, e.message)
      return {'CANCELLED'}
  
def register():
  bpy.utils.register_class(ToxicBlend_BoostSimplify)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_BoostSimplify)

if __name__ == "__main__":
  register()