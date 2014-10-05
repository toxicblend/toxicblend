#!/usr/bin/python   
import bpy
import toxicblend
import imp

bl_info = {
  "name": "Toxicblend - Generate Mesh",
  'description': 'Toxiclibs.generatemesh: Generates mesh from edge rings',
  'author': 'EAD Fritz',
  'blender': (2, 71, 0),
  "category": "Object",
}
       
class ToxicBlend_GenerateMesh(bpy.types.Operator):
  '''Calls toxiclibs.generatemesh '''
  bl_idname = "object.toxicblend_meshgenerator"
  bl_label = "Toxicblend:GenerateMesh"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  
  useMultiThreadingProperty = bpy.props.EnumProperty(
    description="Each continous ring segment will be processed in a separate thread",
    name="Use mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           default="FALSE"    
          )
  useToOutlineProperty = bpy.props.EnumProperty(
    description="Execute Polygon2D.toOutline on the result (toxiclibs). Useful when there are self-intersecting loops",
    name="Use toOutline",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           default="FALSE"    
          )
                    
  radiusProperty = bpy.props.FloatProperty(name="Radius [mm]", description="The radius of the curvature (in mm)", default=2.0, min=0.0, max=10000.0)

  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend)
    try:
      with toxicblend.ByteCommunicator("localhost", 9999) as bc: 
        unitSystemProperty = context.scene.unit_settings
        activeObject = context.scene.objects.active
        properties = {'useMultiThreading' : str(self.useMultiThreadingProperty),
                      'useToOutline'      : str(self.useToOutlineProperty),
                      'radius'            : str(self.radiusProperty),
                      'unitSystem'        : str(unitSystemProperty.system), 
                      'unitScale'         : str(unitSystemProperty.scale_length) }
                     
        bc.sendMultipleBlenderObjects(bpy.context.selected_objects, self.bl_idname, properties) 
        bc.receiveObjects()
        return {'FINISHED'}
    except toxicblend.ToxicblendException as e:
      self.report({'ERROR'}, e.message)
      return {'CANCELLED'}
  
def register():
  bpy.utils.register_class(ToxicBlend_GenerateMesh)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_GenerateMesh)

if __name__ == "__main__":
  register()
  
