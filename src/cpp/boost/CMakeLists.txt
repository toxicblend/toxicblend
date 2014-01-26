cmake_minimum_required(VERSION 2.8.12)
SET(BOOST_MIN_VERSION "1.55.0")

project( toxicblend-boost1 )

set(Boost_INCLUDE_DIR /opt/local/include)
set(Boost_LIBRARY_DIR /opt/local/lib)
FIND_PACKAGE(Boost ${BOOST_MIN_VERSION} COMPONENTS system filesystem REQUIRED)
FIND_PACKAGE(Java COMPONENTS Development REQUIRED)
FIND_PACKAGE(JNI REQUIRED)

include(UseJava)

if (JNI_FOUND)
    message (STATUS "JNI_INCLUDE_DIRS=${JNI_INCLUDE_DIRS}")
    message (STATUS "JNI_LIBRARIES=${JNI_LIBRARIES}")
    # Java_JAR_EXECUTABLE points to ANY old file named jar, even non-executable ones!
    message (STATUS "jar command=${Java_JAR_EXECUTABLE}")
endif()

include_directories(${Boost_INCLUDE_DIR} ${JNI_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIR})

add_library(toxicblendboost SHARED medianaxis.cpp voronoiedges.cpp org_toxicblend_operations_boostmedianaxis_MedianAxisJni.cpp)
target_link_libraries( toxicblendboost ${Boost_LIBRARIES} ${JNI_LIBRARIES})

get_property(sharedlibrary_file TARGET toxicblendboost PROPERTY LOCATION)
get_filename_component(sharedlibrary_filename ${sharedlibrary_file} NAME)
get_filename_component(sharedlibrary_dir ${sharedlibrary_file} DIRECTORY)
SET(sharedlibrary_jar "${sharedlibrary_dir}/toxicblendboost.jar")

message (STATUS "sharedlibrary_file == ${sharedlibrary_file}")
message (STATUS "sharedlibrary_dir == ${sharedlibrary_dir}")
message (STATUS "sharedlibrary_filename == ${sharedlibrary_filename}")
message (STATUS "sharedlibrary_jar == ${sharedlibrary_jar}")

# This does not work because add_jar always adds libraries by their full path - useless
# http://public.kitware.com/Bug/view.php?id=14500
# add_jar( toxicblendboostjar 
#         SOURCES ${sharedlibrary_filename} 
#         OUTPUT_NAME toxicblendboost
#         #OUTPUT_DIR ./../../lib 
#          )


ADD_CUSTOM_COMMAND(TARGET toxicblendboost
                   POST_BUILD
                   COMMAND cmake -E echo "Creating jar file..."
                   COMMAND ${Java_JAR_EXECUTABLE} cvf ${sharedlibrary_jar} -C ${sharedlibrary_dir} ${sharedlibrary_filename}
)
