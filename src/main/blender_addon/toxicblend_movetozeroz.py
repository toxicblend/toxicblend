import bpy
from mathutils import Matrix, Vector

bl_info = {
  "name": "Moves an object so that top of bounding box touches the Z=0 plane",
  "category": "Object",
}

class ToxicBlend_MoveToZeroZOperator(bpy.types.Operator):
  '''Moves an object so that top of bounding box touches the Z=0 plane'''
  bl_idname = "object.toxicblend_movetozeroz"
  bl_label = "ToxicBlend Move to zero Z"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.
    
  @classmethod
  def poll(cls, context):
    return context.active_object is not None
      
  def execute(self, context):
    activeObject = bpy.context.scene.objects.active
    matrix = Matrix(activeObject.matrix_world)
    highestPoint = matrix * Vector(activeObject.bound_box[0])
    for i in range(1,8):
      v = matrix * Vector(activeObject.bound_box[i])
      if v.z > highestPoint.z:
        highestPoint = v     
    activeObject.location[2] = activeObject.location[2]-highestPoint.z
    return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_MoveToZeroZOperator)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_MoveToZeroZOperator)

if __name__ == "__main__":
  register()
