#!/usr/bin/python   
import bpy
import toxicblend
import imp

# How to install this plugin:
# 
# run this in the blender console:
#   import site; site.getsitepackages()
#
# copy the content of the toxicblend/src/main/blender_addon/site-packages directory to one of the 
# directories listed by the previous command. 
# 
# OSX example:
# cp -R toxicblend/src/main/blender_addon/site-packages/* /Applications/Blender-2.72b/blender-2.72b.app/Contents/MacOS/../Resources/2.72/python/lib/python3.4/site-packages
#
# then restart blender and use "Run script" on this file

bl_info = {
  "name": "Toxicblend - Generate Mesh",
  'description': 'Toxiclibs.generatemesh: Generates mesh from edge rings',
  'author': 'EAD Fritz',
  'blender': (2, 72, 0),
  "category": "Object",
}
       
class ToxicBlend_GenerateMesh(bpy.types.Operator):
  '''Calls toxicblend.generatemesh '''
  bl_idname = "object.toxicblend_meshgenerator"
  bl_label = "Toxicblend:GenerateMesh"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  subdivisionProperty = bpy.props.IntProperty(name="sub divisions", description="The number of sub divisions", default=5, min=1, max=100)
  radius1Property = bpy.props.FloatProperty(name="radius clipping low", description="r will be interpolated to fit inside relative radius", default=0.0, min=0, max=1.0)
  radius2Property = bpy.props.FloatProperty(name="radius clipping high", description="r will be interpolated to fit inside relative radius", default=1.0, min=0, max=1.0)
  zAlgorithmProperty = bpy.props.EnumProperty(
    description="Z coordinate is calculated by:",
    name="Use mulithreading algorithm",
    items=(("CIRCLEINTERSECTION", "Circle intersection","z=sqrt((4d*r*r-d*d)/4d)"),
           ("CIRCLEARC", "Arc of a circle","z=sqrt(1-r*r)")),
           default="CIRCLEARC" )
  useMultiThreadingProperty = bpy.props.EnumProperty(
    description="Sections of the computations will be processed in a separate threads",
    name="Use mulithreading algorithm",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           default="FALSE" )
             
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend)
    try:
      with toxicblend.ByteCommunicator("localhost", 9999) as bc: 
        unitSystemProperty = context.scene.unit_settings
        activeObject = context.scene.objects.active
        properties = {'useMultiThreading' : self.useMultiThreadingProperty,
                      'zAlgorithm'        : self.zAlgorithmProperty,
                      'subdivisions'      : str(self.subdivisionProperty),
                      'radius1Property'   : str(self.radius1Property),
                      'radius2Property'   : str(self.radius2Property),
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
  
