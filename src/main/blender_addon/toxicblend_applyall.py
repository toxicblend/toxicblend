import bpy

bl_info = {
  'name': "Toxicblend - Apply all transformations (standalone)",
  'description': 'Apply all world transformations (rotation, scala, location) on a model (standalone)',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
  'location': 'View3D > Object > Apply',
}

class ToxicBlend_ApplyAllTransforms(bpy.types.Operator):
  '''Apply all transformations (rotation, scala, location)'''
  bl_idname = "object.toxicblend_apply_all_transforms"
  bl_label = "Toxicblend:Apply all transformations"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
    
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    bpy.ops.object.transform_apply(location=True,rotation=True,scale=True)
    return {'FINISHED'} 
    
def menu_func(self, context):
  self.layout.operator(ToxicBlend_ApplyAllTransforms.bl_idname, text=ToxicBlend_ApplyAllTransforms.bl_label)
    
def register():
  bpy.utils.register_module(__name__)
  bpy.types.VIEW3D_MT_object_apply.append(menu_func)
 
def unregister():
  bpy.utils.unregister_module(__name__)
  bpy.types.VIEW3D_MT_object_apply.remove(menu_func)
    
if __name__ == "__main__":
  register()    