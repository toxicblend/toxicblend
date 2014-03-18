#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

bl_info = {
  "name": "Toxicblend - Intersect Edges",
  'description':' Merges two objects made of edges. Self-intersecting edges inside one of the objects are ignored',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
          
class ToxicBlend_IntersectEdges(bpy.types.Operator):
  '''Intersect edges'''
  bl_idname = "object.toxicblend_intersectedges"
  bl_label = "Toxicblend:Intersect Edges"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
  multiThreadProperty = bpy.props.EnumProperty(
    name="Use experimental multi threading",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           #update=mode_update_callback
           default="TRUE")
                     
  @classmethod
  def poll(cls, context):
    return context.active_object is not None and len(bpy.context.selected_objects) > 1

  def execute(self, context):
    if len(bpy.context.selected_objects) < 2:
      self.report({'ERROR'}, 'You must select more than one object.')
      return {'CANCELLED'}
     
    try:
      imp.reload(toxicblend) # needed when reloading toxicblend site-packages, won't be used in a release version
      with toxicblend.ByteCommunicator("localhost", 9999) as bc: 
        
        activeObject = context.scene.objects.active
        unitSystemProperty = context.scene.unit_settings
        
        properties = {'useMultiThreading' : str(self.multiThreadProperty),
                      'unitSystem'        : str(unitSystemProperty.system), 
                      'unitScale'         : str(unitSystemProperty.scale_length)}
        
        bc.sendMultipleBlenderObjects(bpy.context.selected_objects, self.bl_idname, properties) 
        bc.receiveObjects(removeDoublesThreshold=0.00001)
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
 
  bpy.utils.register_class(ToxicBlend_IntersectEdges)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_IntersectEdges)

if __name__ == "__main__":
  register()