import bpy
from mathutils import Matrix, Vector

bl_info = {
  "name": "Toxicblend - Move to zero Z (standalone)",
  'description': 'Moves all selected objects so that top of thier highest bounding box touches the Z=0 plane (standalone). Note that this operates on the bounding box of the objects, so you should probably apply the model transformation first.',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}

class ToxicBlend_MoveToZeroZOperator(bpy.types.Operator):
  '''Moves all selected objects so that top of thier highest bounding box touches the Z=0 plane (standalone)'''
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
    selectedObjects = bpy.context.selected_objects

    matrix = Matrix(activeObject.matrix_world)
    highestPoint = matrix * Vector(activeObject.bound_box[0])
    lowestPoint = matrix * Vector(activeObject.bound_box[0])
    for o in selectedObjects:
      for i in range(1,8):
        v = matrix * Vector(o.bound_box[i])
        if v.z > highestPoint.z:
          highestPoint = v
        if v.x < lowestPoint.x:
          lowestPoint.x = v.x
        if v.y < lowestPoint.y:
          lowestPoint.y = v.y
        if v.z < lowestPoint.z:
          lowestPoint.z = v.z

    for o in selectedObjects:    
      if self.moveXYProperty=="TRUE":
        o.location[0] = o.location[0]-lowestPoint.x
        o.location[1] = o.location[1]-lowestPoint.y
      
      o.location[2] = o.location[2]-highestPoint.z
    return {'FINISHED'}

def register():
  bpy.utils.register_class(ToxicBlend_MoveToZeroZOperator)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_MoveToZeroZOperator)

if __name__ == "__main__":
  register()