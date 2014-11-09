#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

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
  "name": "Toxicblend - Z adjust",
  'description':'Adjusts one object with the z-adjust from another.',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
          
class ToxicBlend_ZAdjust(bpy.types.Operator):
  '''Z adjust'''
  bl_idname = "object.toxicblend_zadjust"
  bl_label = "Toxicblend:Z adjust"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  sampleStepProperty = bpy.props.FloatProperty(name="Sample step [mm]", default=0.05, min=0.001, max=100, description="the distance between sample in mm.")  
  multiThreadProperty = bpy.props.EnumProperty(
    name="Use experimental multi threading",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           default="TRUE")       
  addDiffProperty = bpy.props.EnumProperty(
    name="Add diff",
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
        activeObject = context.scene.objects.active
        unitSystemProperty = context.scene.unit_settings
        properties = {'sampleStep'        : str(self.sampleStepProperty),
                      'useMultiThreading' : str(self.multiThreadProperty),
                      'addDiff'           : str(self.addDiffProperty),
                      'unitSystem'        : str(unitSystemProperty.system),
                      'unitScale'         : str(unitSystemProperty.scale_length)}
        bc.sendMultipleBlenderObjects(bpy.context.selected_objects, self.bl_idname, properties) 
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
 
  bpy.utils.register_class(ToxicBlend_ZAdjust)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_ZAdjust)

if __name__ == "__main__":
  register()