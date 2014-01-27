cmake_minimum_required(VERSION 2.8.11)
SET(BOOST_MIN_VERSION "1.55")

project( toxicblend-boost1 )
set(BOOST_DIR "$ENV{BOOST_HOME}")
if(BOOST_DIR)               # <--- Use 'BOOST_DIR', not 'DEFINED ${BOOST_DIR}'
    #global include directories
    include_directories(${BOOST_DIR})
else()
    message(STATUS "BOOST_HOME at ${BOOST_DIR}")
    message(FATAL_ERROR "Undefined BOOST_HOME env var.")
endif()

#set(Boost_INCLUDE_DIR /opt/local/include)
#set(Boost_LIBRARY_DIR /opt/local/lib)
FIND_PACKAGE(Boost ${BOOST_MIN_VERSION} COMPONENTS system REQUIRED)
FIND_PACKAGE(Java COMPONENTS Development REQUIRED)
FIND_PACKAGE(JNI REQUIRED)

include(UseJava)

if (JNI_FOUND)
  message (STATUS "JNI_INCLUDE_DIRS=${JNI_INCLUDE_DIRS}")
  message (STATUS "JNI_LIBRARIES=${JNI_LIBRARIES}")
 # Java_JAR_EXECUTABLE points to ANY old file named jar, even non-executable ones!
  message (STATUS "jar command=${Java_JAR_EXECUTABLE}")
endif()
# on linux I get this annoying error:
# Exception in thread "main" java.lang.UnsatisfiedLinkError:
# libvtkgdcmJava.so:
# libmawt.so: cannot open shared object file: No such file or directory

# let's find this lib here then
if(UNIX)
  find_library(JAVA_MAWT_LIBRARY NAMES mawt
    # there is one also in headless but it does not work...
    PATHS ${JavaProp_SUN_BOOT_LIBRARY_PATH}/xawt
  )
endif()

include_directories(${Boost_INCLUDE_DIR} ${JNI_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIR})

add_library(toxicblendboost SHARED medianaxis.cpp voronoiedges.cpp org_toxicblend_operations_boostmedianaxis_MedianAxisJni.cpp)
target_link_libraries( toxicblendboost ${Boost_LIBRARIES} ${JNI_LIBRARIES})

get_property(sharedlibrary_file TARGET toxicblendboost PROPERTY LOCATION)
get_filename_component(sharedlibrary_filename ${sharedlibrary_file} NAME)
IF (${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}.${CMAKE_PATCH_VERSION} GREATER 2.8.11)
  get_filename_component(sharedlibrary_dir ${sharedlibrary_file} DIRECTORY)
ELSE (${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}.${CMAKE_PATCH_VERSION} GREATER 2.8.11)
  get_filename_component(sharedlibrary_dir ${sharedlibrary_file} PATH)
ENDIF (${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}.${CMAKE_PATCH_VERSION} GREATER 2.8.11)
get_filename_component(sharedlibrary_ext ${sharedlibrary_file} EXT)
get_filename_component(sharedlibrary_we ${sharedlibrary_file} NAME_WE)

SET(sharedlibrary_jar "${sharedlibrary_dir}/toxicblendboost.jar")

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  if (NOT ${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
     set(sharedlibrary_file "${sharedlibrary_dir}/${sharedlibrary_we}64${sharedlibrary_ext}" )
     set_property(TARGET toxicblendboost PROPERTY PREFIX "")     
     set_property(TARGET toxicblendboost PROPERTY OUTPUT_NAME "${sharedlibrary_we}64" )
     get_property(sharedlibrary_file TARGET toxicblendboost PROPERTY LOCATION)
     get_filename_component(sharedlibrary_filename ${sharedlibrary_file} NAME)
  endif()
endif()

message (STATUS "sharedlibrary_file == ${sharedlibrary_file}")
message (STATUS "sharedlibrary_dir == ${sharedlibrary_dir}")
message (STATUS "sharedlibrary_filename == ${sharedlibrary_filename}")
message (STATUS "sharedlibrary_jar == ${sharedlibrary_jar}")
message (STATUS "sharedlibrary_ext == ${sharedlibrary_ext}")


# This does not work because add_jar always store the full path of libraries inside the jar - totally useless
# http://public.kitware.com/Bug/view.php?id=14500
# add_jar( toxicblendboostjar 
#         SOURCES ${sharedlibrary_filename} 
#         OUTPUT_NAME toxicblendboost
#         #OUTPUT_DIR ./../../lib 
#          )

ADD_CUSTOM_COMMAND(
  TARGET toxicblendboost
  POST_BUILD
  COMMAND cmake -E echo "Creating jar file..."
  COMMAND ${Java_JAR_EXECUTABLE} cvf ${sharedlibrary_jar} -C ${sharedlibrary_dir} ${sharedlibrary_filename}
)
