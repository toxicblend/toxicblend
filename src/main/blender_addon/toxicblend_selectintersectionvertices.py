import bpy
import bmesh
import array

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
  'name': "Toxicblend - Select intersection vertices (standalone)",
  'description': 'Selects all vertices that are connected to more than two other vertices (standalone)',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  'category': 'Mesh',
  'location': 'View3D > EditMode > Select',
}

class ToxicBlend_SelectIntersectionVertices(bpy.types.Operator):
  '''Selects all vertices that are connected to more than two other vertices (standalone)'''
  bl_idname = "object.toxicblend_select_intersection_vertices"
  bl_label = "Toxicblend:Select intersection vertices"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
    
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
      
    # Get the active mesh
    obj = bpy.context.edit_object
    me = obj.data

    # Get a BMesh representation
    bm = bmesh.from_edit_mesh(me)
    bpy.ops.mesh.select_all(action='DESELECT')   
        
    if len(bm.edges) > 0 or len(bm.faces) > 0:
      verticeConnections = array.array('i',(0 for i in range(0,len(bm.verts))))
      for e in bm.edges:
        for vi in e.verts:
          verticeConnections[vi.index] += 1
      for f in bm.faces:
        for vi in f.verts:
          verticeConnections[vi.index] += 1  
            
      for vi in range(0,len(verticeConnections)):
        if (verticeConnections[vi] > 2):
          bm.verts[vi].select = True     
   
    # Show the updates in the viewport
    bmesh.update_edit_mesh(me, False)

    return {'FINISHED'}

def menu_func(self, context):
  self.layout.operator(ToxicBlend_SelectIntersectionVertices.bl_idname, text=ToxicBlend_SelectIntersectionVertices.bl_label)
    
def register():
  bpy.utils.register_module(__name__)
  bpy.types.VIEW3D_MT_select_edit_mesh.append(menu_func)
 
def unregister():
  bpy.utils.unregister_module(__name__)
  bpy.types.VIEW3D_MT_select_edit_mesh.remove(menu_func)
    
if __name__ == "__main__":
  unregister()
  register()    