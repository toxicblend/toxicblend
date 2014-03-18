import bpy
from mathutils import Matrix, Vector

bl_info = {
  "name": "Toxicblend - Move to zero Z (standalone)",
  'description': 'Moves an object so that top of bounding box touches the Z=0 plane (standalone)',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}

class ToxicBlend_MoveToZeroZOperator(bpy.types.Operator):
  '''Moves an object so that top of bounding box touches the Z=0 plane'''
  bl_idname = "object.toxicblend_movetozeroz"
  bl_label = "Toxicblend:Move to zero Z"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.

  moveXYProperty = bpy.props.EnumProperty(
    name="Move to smallest X & Y as well",
    items=(("TRUE", "True",""),
           ("FALSE", "False","")),
           default="FALSE"    
          )

  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    activeObject = bpy.context.scene.objects.active
    matrix = Matrix(activeObject.matrix_world)
    highestPoint = matrix * Vector(activeObject.bound_box[0])
    lowestPoint = matrix * Vector(activeObject.bound_box[0])
    for i in range(1,8):
      v = matrix * Vector(activeObject.bound_box[i])
      if v.z > highestPoint.z:
        highestPoint = v
      if v.x < lowestPoint.x:
        lowestPoint.x = v.x
      if v.y < lowestPoint.y:
        lowestPoint.y = v.y
      if v.z < lowestPoint.z:
        lowestPoint.z = v.z
        
    if self.moveXYProperty=="TRUE":
      activeObject.location[0] = activeObject.location[0]-lowestPoint.x
      activeObject.location[1] = activeObject.location[1]-lowestPoint.y
      
    activeObject.location[2] = activeObject.location[2]-highestPoint.z
    return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_MoveToZeroZOperator)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_MoveToZeroZOperator)

if __name__ == "__main__":
  register()