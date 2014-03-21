#!/usr/bin/python   
import bpy
import toxicblend
import imp # needed when reloading toxicblend site-packages, won't be used in a release version

bl_info = {
  "name": "Toxicblend - Subdivide Edges",
  'description': 'Subdivides edges by length',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
       
class ToxicBlend_SubdivideEdges(bpy.types.Operator):
  '''Subdivide edges'''
  bl_idname = "object.toxicblend_subdivideedges"
  bl_label = "Toxicblend:Subdivide Edges"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
          
  segmentLengthProperty = bpy.props.FloatProperty(name="Segment length [mm]", default=1.0, min=0.00001, max=9000, description="Subdivide edges so that they are no longer than his.")  
  
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    imp.reload(toxicblend)
    try:
      with toxicblend.ByteCommunicator("localhost", 9999) as bc: 
        unitSystemProperty = context.scene.unit_settings
        activeObject = context.scene.objects.active
        properties = {'segmentLength' : str(self.segmentLengthProperty),
                      'unitSystem'    : str(unitSystemProperty.system), 
                      'unitScale'     : str(unitSystemProperty.scale_length) }
                     
        bc.sendSingleBlenderObject(activeObject, self.bl_idname, properties) 
        # remove doubles with mm as unit and half the resolution of the simplify operation
        bc.receiveObjects() #removeDoublesThreshold=self.simplifyLimitProperty/(unitSystemProperty.scale_length*2000.))
        return {'FINISHED'}
    except toxicblend.ToxicblendException as e:
      self.report({'ERROR'}, e.message)
      return {'CANCELLED'}
  
def register():
  bpy.utils.register_class(ToxicBlend_SubdivideEdges)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_SubdivideEdges)

if __name__ == "__main__":
  register()