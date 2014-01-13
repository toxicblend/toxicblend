//
//  medianAxisjni.h
//
//  Created by Daniel on 2013-05-20.
//  Copyright (c) 2013 None. All rights reserved.
//


#ifndef medianaxis_medianaxis_h
#define medianaxis_medianaxis_h

#include <iostream>
#include <map>
#include <boost/noncopyable.hpp>
#include <jni.h>
#include <string>
#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <boost/geometry/geometries/adapted/c_array.hpp>
#include <boost/geometry/geometry.hpp>
#include <boost/geometry/algorithms/disjoint.hpp>

//#define MEDIANAXISDEBUG 1
#ifdef MEDIANAXISDEBUG
#define DBG
#else
#define DBG for(;0;)
#endif

namespace medianAxisFloat {
  
  typedef double m_coordinate_type;
  typedef boost::geometry::model::d2::point_xy<m_coordinate_type> m_point_type;
  typedef boost::geometry::model::ring<m_point_type> m_ring_type;
  
  template<class T>
  class ArrayContainer: boost::noncopyable{
  public:
    ArrayContainer(T*inData, jsize inLength);
    ~ArrayContainer();
    
    T* data;
    jsize length;
    m_ring_type ring;
  };
  
  typedef ArrayContainer<float> ArrayContainerFloat_t;
  typedef std::map<long, ArrayContainerFloat_t*> int2ptrMap_type;
  typedef int2ptrMap_type::iterator int2ptrMap_typei;
  
  class Medianaxis: boost::noncopyable{
    
  public:
    Medianaxis();
    
    ~Medianaxis();
    jint addRing(jfloat* inCArray, jsize size, jfloat limit);
    ArrayContainerFloat_t* getRing(jint ringId);
    void setRing(jint ringId, jfloat* inCArray, jsize size, jfloat limit);
    bool ringContainsAllPoints(jfloat* inCArray, jsize size, jint ringId);
    bool ringContainsRing(jint outerRingId, jint innerRingId);
    void voronoiInternalEdges(jint* outerRingIds, jsize outerRingIdsLength, jint* internalRingIds, jsize internalRingIdsLength, jfloat*& outCArray, jsize& outCArrayLength, jfloat zEpsilon, jfloat dotProductLimit, jfloat resolution);
    std::string ring2d2String(jint ringId);
    bool ringIsIntersectingItself(jint ringId);
    std::string edge3d2String(jfloat* edges, jint size);

  protected:
    int2ptrMap_type vertexMap;
    jint ringIdCounter;
  };
}
#endif