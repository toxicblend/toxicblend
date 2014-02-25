import bpy
import bmesh

bl_info = {
  'name': "Toxicblend - Select end vertices (standalone)",
  'description': 'Selects all vertices that are only connected to one other vertice (standalone)',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "3D View",
}

class ToxicBlend_SelectEndVertices(bpy.types.Operator):
  '''Selects all vertices that are only connected to one other vertice (standalone)'''
  bl_idname = "object.toxicblend_select_end_vertices"
  bl_label = "Toxicblend:Select end vertices"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
    
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    bm = bmesh.new()
    me = bpy.context.active_object.data
    bm.from_mesh(me)
    if len(bm.edges) > 0 or len(bm.faces) > 0:
      verticeConnections = [0 for i in me.vertices]
      for e in me.edges:
        for vi in e.vertices:
          verticeConnections[vi] += 1
      for f in bm.faces:
        for vi in f.vertices:
          verticeConnections[vi] += 1  
          
      # do a little dance to make sure that the vertex new selection is visible                
      bpy.ops.object.mode_set(mode='EDIT')
      bpy.ops.mesh.select_all(action='DESELECT')
      bpy.ops.object.mode_set(mode='OBJECT')     
      for vi in range(0,len(verticeConnections)):
        if (verticeConnections[vi] < 2):
          me.vertices[vi].select = True     
      bpy.ops.object.mode_set(mode='EDIT') 
    return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_SelectEndVertices)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_SelectEndVertices)

if __name__ == "__main__":
  register()    