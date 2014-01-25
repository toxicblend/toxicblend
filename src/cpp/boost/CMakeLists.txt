cmake_minimum_required(VERSION 2.8)
project( toxicblend-boost1 )

set(Boost_INCLUDE_DIR /opt/local/include)
set(Boost_LIBRARY_DIR /opt/local/lib)
find_package(Boost COMPONENTS system filesystem REQUIRED)
FIND_PACKAGE(Java COMPONENTS Development REQUIRED)
FIND_PACKAGE(JNI REQUIRED)
#SET(Java_JAR_EXECUTABLE2  $ENV{JAVA_HOME}/bin/jar)
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

get_property(sharedlibrary_location TARGET toxicblendboost PROPERTY LOCATION)
message (STATUS "sharedlibrary_location == ${sharedlibrary_location}")

#UseJava
add_jar( toxicblendboostjar 
         SOURCES ${sharedlibrary_location} 
         OUTPUT_NAME toxicblendboost
         #OUTPUT_DIR ./../../lib 
          )

