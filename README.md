Toxicblend
==========

*** Pre-Alpha code ***

A set of plugins that will bring the power of ToxicLibs, Boost Voronoi, jBullet and other geometric libraries to Blender 2.69+.
There are several blender plugins, one for each operation. They send objects (vertexes, edges/faces) over the net (via google 
protocol buffers) to a JVM server. This server then computes the operation (volumetric brush, median axis, 
simplify 2D polygon, etc. etc) and sends the resulting object back to the blender plugin.

I have tested the code on Ubuntu 13.10, Gentoo (64 bit) and OSX 10.9 with mac ports

Dependencies
------------
blender 2.69                                  (only pure python code is used in blender, so blender addon installation is easy)
https://github.com/malthe/google-protobuf.git (google protobufs with python 3 support, only needed when .proto file is modified) 
https://bitbucket.org/ead_fritz/toxiclibs     (toxiclibs with some minor tweaks)
http://jbullet.advel.cz                       (java port of bullet physics (managed by sbt))  
http://www.scalatest.org                      (scala test suite (managed by sbt))
http://www.scala-sbt.org					  (scala build tool)
www.boost.org version 1.55                    (It seems like only 'System' needs to be installed as a library, rest is header files)
two files from http://libgdx.badlogicgames.com is blatantly copied into the repository
 
Installation & running
----------------------

1. Download and compile the bitbucket.org/ead_fritz/toxiclibs code

	hg clone https://bitbucket.org/ead_fritz/toxiclibs

	cd toxiclibs

	ant -f ant/build_all.xml 

	make soft links from toxicblend/lib/toxiclibscore.jar -> your_compilation_path/toxiclibs/dist/toxiclibscore.jar 
	and toxicblend/lib/volumeutils.jar ->  your_compilation_path/toxiclibs/dist/volumeutils.jar
	
2. Install boost 1.55 (boost.org)
	
	./bootstrap.sh --with-python-version=3.3 --with-libraries=system --prefix=/opt/local
	b2 install
	export BOOST_HOME=/opt/local/
	export BOOST_ROOT=/opt/local/
    (It is only the medianaxis and boostsimplify addon that uses this code, the rest of the addons will work without this step)
		
3. Copy the python files found in src/main/blender_addon/site-packages/ to the site-packages directory of blender.
You will find the location with this command in the blender python console:
	
	import site; site.getsitepackages()

4. then import the blender addons found in src/main/blender_addon to blender.
    Just running them as blender text blocks works.

5. Compile the jni code. 
	
	export JAVA_HOME=...path to java.
        
        example:export JAVA_HOME=/opt/oracle-jdk-bin-1.7.0.51/

	cd toxicblend/src/cpp/boost/
	
	cmake .
	
	make

    (It is only the medianaxis and boostsimplify addon that uses this code, the rest of the addons will work without this step)
	
6. compile the scala and java code using sbt

    cd toxicblend

    sbt
    
    compile  (there are a few warnings generated from libgdx and google-protobuf)
    
7. run the server (still in sbt console)
    
    run
    
    4 (org.toxicblend.Server)

    If you experience problems with a missing java library (libmawt.so in my case)
    you can search for the location and add it to LD_LIBRARY_PATH and restart sbt
    
    find / -name libmawt.so
    
    /opt/oracle-jdk-bin-1.7.0.51/jre/lib/amd64/headless/libmawt.so
    
    /opt/oracle-jdk-bin-1.7.0.51/jre/lib/amd64/xawt/libmawt.so
    
    /opt/icedtea-bin-6.1.12.7/jre/lib/amd64/headless/libmawt.so 
    
    /opt/icedtea-bin-6.1.12.7/jre/lib/amd64/xawt/libmawt.so
    
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/opt/oracle-jdk-bin-1.7.0.51/jre/lib/amd64/xawt/
    
    sbt  
    
8.  Run blender from a terminal 
	
	cd  .../blender-2.69-linux-glibc211-x86_64/
	
	./blender
	
	If any of the addons should hang, you can abort plugin execution with ctrl-c in the terminal.
	
	
8. In blender, press space and search for the addons:
 	
 	
 	toxicblend_volume (converts edges to volumetric voxels) 
 	
 	toxicblend_metavolume (converts edges to volumetric voxels, stand alone pure python) 
    
    toxicblend_applyall (apply all transformations on an object, stand alone pure python) 
    
    toxicblend_dragoncurve (draws a parametric dragon curve as edges)
    
    toxicblend_projectionoutline (orthogonally projects an object on an axis plane)
    
    toxicblend_medianaxis  (Using an experimental median axis algorithm (native boost code))
    
    toxicblend_boostsimplify (Simplifies sequences of edges (native boost code))
    
    toxicblend_simplegcodegenerator (generates layered gcode from a set of edges)
    
    toxicblend_simplegcodeview (visualizes gcode as a set of edges)
    
    toxicblend_zadjust (wraps a set of edges onto objects, similar to blender shrink wrap modifier but with z offset option)
    
    I use the combination of toxicblend_medianaxis, toxicblend_boostsimplify, toxicblend_zadjust and toxicblend_simplegcodegenerator to generate gcode for v-carved text onto curved surfaces.
    
    
    Some day i will document in more detail what the addons actually does and how to use them :)
