*** Pre-Alpha code ***

A set of plugins that will bring the power of ToxicLibs, Boost Voronoi and other geometric libraries to Blender 2.69+.
There are several blender plugins, one for each operation. They send objects (vertexes, edges/faces) over the net (via google 
protocol buffers) to a JVM server. This server then computes the operation (volumetric brush, median axis, 
simplify 2D polygon, etc. etc) and sends the resulting object back to the blender plugin.

I have not yet uploaded:
* the blender plugins
* c++ boost code (jni library)
* google protocol buffer code (will likely be a fork of a python3 pb version i found somewhere)
* slightly modified ToxicLibs library (will likely be a fork)

So none of the plugins are usable yet

Dependencies:
blender 2.69
https://github.com/malthe/google-protobuf.git (google protobufs with python 3 support) 
https://bitbucket.org/ead_fritz/toxiclibs     (toxiclibs with some minor tweaks)
one file: com/badlogic/gdx/utils/SharedLibraryLoader.java from http://libgdx.badlogicgames.com is blatantly copied
www.boost.org version 1.55
 
