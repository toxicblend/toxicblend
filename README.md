Toxicblend
==========

*** Pre-Alpha code ***

A set of plugins that will bring the power of ToxicLibs, Boost Voronoi and other geometric libraries to Blender 2.69+.
There are several blender plugins, one for each operation. They send objects (vertexes, edges/faces) over the net (via google 
protocol buffers) to a JVM server. This server then computes the operation (volumetric brush, median axis, 
simplify 2D polygon, etc. etc) and sends the resulting object back to the blender plugin.

I have tested the code on Ubuntu 13.10, Gentoo (64 bit) and OSX 10.9 with mac ports

Dependencies
------------
blender 2.69 (only pure python code is used in blender, so installation is easy)
https://github.com/malthe/google-protobuf.git (google protobufs with python 3 support, only needed when .proto file is modified) 
https://bitbucket.org/ead_fritz/toxiclibs     (toxiclibs with some minor tweaks)
two files: com/badlogic/gdx/utils/SharedLibraryLoader.java from http://libgdx.badlogicgames.com is blatantly copied.
www.boost.org version 1.55
 
Installation & running
----------------------

1. Download and compile the bitbucket.org/ead_fritz/toxiclibs code

	hg clone https://bitbucket.org/ead_fritz/toxiclibs

	cd toxiclibs

	ant -f ant/build_all.xml 

	make soft links from toxicblend/lib/toxiclibscore.jar -> compilation_path/toxiclibs/dist/toxiclibscore.jar 
	and toxicblend/lib/volumeutils.jar ->  compilation_path/toxiclibs/dist/volumeutils.jar
	
2. Install boost 1.55 (boost.org)
	
	./bootstrap.sh --with-python-version=3.3 --with-libraries=system --prefix=/opt/local
	b2 install
	export BOOST_HOME=/opt/local/
	export BOOST_ROOT=/opt/local/
	
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

	
6. compile the scala and java code using sbt

    cd toxicblend

    sbt
    
    compile  (there are a few warnings generated from gdx and google-protobuf)
    
7. run the server (still in sbt console)
    
    run
    
    4 (org.toxicblend.Server)

    If you experience problems with a missing java library (libmawt.so im my case)
    you can search for the location and add it to LD_LIBRARY_PATH and restart sbt
    
    find / -name libmawt.so
    
    /opt/oracle-jdk-bin-1.7.0.51/jre/lib/amd64/headless/libmawt.so
    
    /opt/oracle-jdk-bin-1.7.0.51/jre/lib/amd64/xawt/libmawt.so
    
    /opt/icedtea-bin-6.1.12.7/jre/lib/amd64/headless/libmawt.so 
    
    /opt/icedtea-bin-6.1.12.7/jre/lib/amd64/xawt/libmawt.so
    
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}/opt/oracle-jdk-bin-1.7.0.51/jre/lib/amd64/xawt/
    
    sbt  
    
8.  Run blender from a terminal 
	
	cd  .../blender-2.69-linux-glibc211-x86_64/
	
	./blender
	
	If any of the plugins should hang, you can abort plugin execution with ctrl-c in the terminal.
	
	
8. In blender, press space and search for the addons:
 	
 	
 	toxicblend_volume
    
    toxicblend_add_dragon_curve
    
    toxicblend_projection_outline
    
    toxicblend_medianaxis
    
    toxicblend_boostsimplify
    
    Some day i will document what the plugins actually does :)
