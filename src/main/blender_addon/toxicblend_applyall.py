import bpy

bl_info = {
  "name": "Apply all transformations (rotation, scala, location)",
  "category": "Object",
}

class ApplyAllTransformsOperator(bpy.types.Operator):
  '''Apply all transformations (rotation, scala, location)'''
  bl_idname = "object.toxicblend_apply_all_transforms"
  bl_label = "ToxicBlend Apply All Transforms"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
    
  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    bpy.ops.object.transform_apply(location=True,rotation=True,scale=True)
    return {'FINISHED'}

def register():
  bpy.utils.register_class(ApplyAllTransformsOperator)

def unregister():
  bpy.utils.unregister_class(ApplyAllTransformsOperator)

if __name__ == "__main__":
  register()
