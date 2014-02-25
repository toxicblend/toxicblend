#!/usr/bin/python   
import bpy
import bmesh

bl_info = {
  "name": "Toxicblend - DebugObj (standalone)",
  'description': 'Checks selected object for anomalies, and prints the results to the console',
  'author': 'EAD Fritz',
  'blender': (2, 69, 0),
  "category": "Object",
}
          
class ToxicBlend_DebugObj(bpy.types.Operator):
  '''Simple object debug'''
  bl_idname = "object.toxicblend_debugobj"
  bl_label = "Toxicblend:debug object (check console for messages)"
  bl_options = {'REGISTER', 'UNDO'}  # enable undo for the operator.

  @classmethod
  def poll(cls, context):
    return context.active_object is not None

  def execute(self, context):
    activeObject = context.scene.objects.active
    bm = bmesh.new()
    bm.from_mesh(activeObject.data)
    objectName = activeObject.name
    print("ToxicBlend_DebugObj:") 
    print("vertices:")
    for voi in range (0,len(bm.verts)):
      v = bm.verts[voi]     
      if v.index == voi:
        print("%d:(%f, %f, %f)" % (v.index, v.co.x, v.co.y, v.co.z))
      else:
        print("%d:(%f, %f, %f) %s %d" % (v.index, v.co.x, v.co.y, v.co.z, "index does not match position:", voi))  
    
    if len(bm.faces)==0:
      print("No faces")
    else:
        print("Faces:")
        for f in bm.faces:
          indices = []
          for v in f.verts:
            indices.append(str(v.index))
          print(",".join(indices))
    
    
    if len(bm.edges)==0:
      print("No edges")
    else:
        print("Edges:") 
        edgeMap = {} 
        for e in bm.edges:
          indices = []
          for v in e.verts:
            indices.append(str(v.index))
          if len(indices)!=2 or indices[0]==indices[1]:
            print("%s %s"%(",".join(indices), "problematic!!" ))  
          else:
            lowI = min(int(indices[0]),int(indices[1]))
            highI = max(int(indices[0]),int(indices[1]))
            #print("%d to %d " % (lowI,highI))
            key = "%d-%d" % (lowI,highI)
            if key in edgeMap:
              print("%s %s"%(",".join(indices), "double edge!!" ))  
            else:
              print(",".join(indices))
            edgeMap[key] = True    
            
    return {'FINISHED'}

def register():
  # Check Blender version
  req = [2, 69, 0]
  (a,b,c) = bpy.app.version
  if a < req[0] or (a==req[0] and b<req[1]) or (a==req[0] and b == req[1] and c < req[2]):
    msg = 'Blender too old: %s < %s' % ((a,b,c), tuple(version))
    raise NameError(msg)
 
  bpy.utils.register_class(ToxicBlend_DebugObj)

def unregister():
  bpy.utils.unregister_class(ToxicBlend_DebugObj)

if __name__ == "__main__":
  register()
