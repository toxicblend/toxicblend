import bpy

bl_info = {
  'name': "Toxicblend - Apply all transformations (standalone)",
  'description': 'Apply all world transformations (rotation, scala, location) on a model (standalone)',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
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

def register():
  bpy.utils.register_class(ToxicBlend_ApplyAllTransforms)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_ApplyAllTransforms)

if __name__ == "__main__":
  register()
