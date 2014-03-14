#!/usr/bin/python   
import bpy
import toxicblend

# import site; site.getsitepackages()

import imp

bl_info = {
  "name": "Toxicblend - Offset 2D",
  'description': 'Toxiclibs.Polygon2d.offset',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
       
class ToxicBlend_Offset2d(bpy.types.Operator):
  '''Naive implementation of median axis'''
  bl_idname = "object.toxicblend_offset2d"
  bl_label = "Toxicblend:Offset2D"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  projectionPlaneProperty = bpy.props.EnumProperty(
    name="Choose 2D plane projection",
    description = "For now manual projection selection will be used.",
    items=(("YZ_PLANE", "YZ",""),
           ("XZ_PLANE", "XZ",""), 
           ("XY_PLANE", "XY","")),
           #update=mode_update_callback
           default="XY_PLANE"    
          )
  useMultiThreadingProperty = bpy.props.EnumProperty(
    description="Each continous ring segment will be processed in a separate thread",
    name="Use mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           #update=mode_update_callback
           default="FALSE"    
          )
          
  offsetProperty = bpy.props.FloatProperty(name="Offset", description="The value (in mm) you want to shrink or expand an edge ring", default=2.0, min=-10000.0, max=10000.0)

  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend)
    with toxicblend.ByteCommunicator("localhost", 9999) as c: 
      # bpy.context.selected_objects,
      unitSystemProperty = context.scene.unit_settings
      activeObject = context.scene.objects.active
      properties = {'projectionPlane'       : str(self.projectionPlaneProperty), 
                    'useMultiThreading'     : str(self.useMultiThreadingProperty),
                    'offset'                :  str(self.offsetProperty),
                    'unitSystem'            : str(unitSystemProperty.system), 
                    'unitScale'             : str(unitSystemProperty.scale_length) }
                     
      c.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
      c.receiveObjects()
      return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_Offset2d)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_Offset2d)

if __name__ == "__main__":
  register()
  