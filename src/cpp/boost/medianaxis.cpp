//
//  medianAxis.cpp
//
//  Created by EAD Fritz on 2013-05-20.
//  Copyright (c) 2013 EAD Fritz. All rights reserved.
//

#include "medianaxis.hpp"

#include <vector>
#include "voronoiedges.hpp"
#include <string>
#include <boost/geometry.hpp>
#include <boost/geometry/geometry.hpp>
#include <boost/geometry/geometries/linestring.hpp>
#include "boost/format.hpp"
#include <boost/geometry/geometries/point_xy.hpp>

namespace medianAxisFloat {
  
  void medianAxis_ring_helper(m_ring_type& poly, double *inCArray, jsize inCArray_size ){
    for (int i=0; i<inCArray_size; i+=2){
      m_point_type p(inCArray[i],inCArray[i+1]);
      std::cout << p.x() << " " << p.y() << std::endl;
      boost::geometry::append( poly, p) ;
    }
    std::cout << std::endl;
  }
  
  template<class T>
  ArrayContainer<T>::ArrayContainer(T* inData, jsize inLength):ring(){
    DBG std::cout << "ArrayContainer new object: points " << inLength/2 << std::endl;
    
    for (int i=0; i<inLength; i+=2){
      m_point_type p(inData[i],inData[i+1]);
      //std::cout << "ArrayContainer new object: points " << p.x() << "," << p.y() << std::endl;
      boost::geometry::append( ring, p) ;
    }
    boost::geometry::correct(ring);
    
    jsize newLength = (jsize) boost::geometry::num_points(ring)*2;
    if (newLength!=inLength){
      DBG std::cout << "ArrayContainer new object: corrected points " << newLength/2 << std::endl;
    }
    data = new T[newLength];
    int i=0;
    for (m_ring_type::iterator j=ring.begin(); j<ring.end(); i+=2, j++){
      data[i]=j->x();
      data[i+1]=j->y();
      //std::cout << "ArrayContainer new object: corrected points " << j->x() << "," << j->y() << " i " << i << std::endl;
    }
    length=newLength;
  }
  
  template<class T>
  ArrayContainer<T>::~ArrayContainer(){
    std::cout << "ArrayContainer delete " << std::endl;
    length = 0;
    delete [] data;
    data=NULL;
  }
  
  
  /**
   * Medianaxis
   */
  Medianaxis::Medianaxis():vertexMap(), ringIdCounter(0){
    DBG std::cout << "Medianaxis new object " << std::endl;
  }
  
  Medianaxis::~Medianaxis(){
    DBG std::cout << "C++ Medianaxis delete. ringIdCounter = " << ringIdCounter << std::endl;
    for (int2ptrMap_typei i = vertexMap.begin(); i != vertexMap.end(); i++) {
      DBG std::cout << "C++ Medianaxis delete. deleting ringId:" << i->first << std::endl;
    	delete i->second;
    }
  }
  
  std::string Medianaxis::ring2d2String(jint ringId){
  	std::string rv;
    ArrayContainerFloat_t* acp = getRing(ringId);
    if (NULL!=acp) {
      for (m_ring_type::iterator i=acp->ring.begin(); i<acp->ring.end(); i++){
        rv += str(boost::format("(%1%,%2%)") % i->x() % i->y());
        //rv += "(" + i->x() + "," i->y() + ")";
      }
    }
    return rv;
  }
  
  std::string Medianaxis::edge3d2String(jfloat* edges, jint size){
    std::string rv;
    for (int i=0; i<size;i+=3){
      if (isnan(edges[i])) {
        rv += "\n";
        i-=2;
        continue;
      }
      rv += str(boost::format("(%1%,%2%,%3%)") % edges[i] % edges[i+1] % edges[i+2]);
    }
    return rv;
  }
  
  jint Medianaxis::addRing(jfloat* inCArray, jsize size, jfloat simplifylimit){
    jint ringId=0;
    int2ptrMap_typei i;
    while (true){
      ringId = ++ringIdCounter;
      i = vertexMap.find(ringId);
      if(i == vertexMap.end()) {
        vertexMap[ringId] = new ArrayContainerFloat_t(inCArray, size);
        return ringId;
      }
    }
  }

  bool Medianaxis::ringIsIntersectingItself(jint ringId){
    int2ptrMap_typei i = vertexMap.find(ringId);
    if(i == vertexMap.end()) {
      std::cerr << "c++ Medianaxis::ringIsIntersectingItself: Could not find ringId:" << ringId << std::endl;
      return false;
    } else {
      return boost::geometry::intersects(i->second->ring);
    }
  }

  ArrayContainerFloat_t* Medianaxis::getRing(jint ringId){
    int2ptrMap_typei i = vertexMap.find(ringId);
    if(i == vertexMap.end()) {
      return NULL;
    } else {
      return i->second;
    }
  }
    
  void Medianaxis::setRing(jint ringId, jfloat* inCArray, jsize size, jfloat simplifylimit){
    int2ptrMap_typei i = vertexMap.find(ringId);
    if(i != vertexMap.end()) {
      delete i->second;
      vertexMap.erase(i);
    }
    vertexMap[ringId] = new ArrayContainerFloat_t(inCArray, size);
  }
  
  bool Medianaxis::ringContainsAllPoints(jfloat* inCArray, jsize size, jint ringId) {
    int2ptrMap_typei i = vertexMap.find(ringId);
    if(i != vertexMap.end()) {
      for (int j=0; j<size; j+=2) {
        m_point_type p(inCArray[j],inCArray[j+1]);
        if (!boost::geometry::within(p,i->second->ring)){
          return false;
        }
      }
    }
    return true;
  }
  
  bool  Medianaxis::ringContainsRing(jint outerRingId, jint innerRingId) {
    std::cerr << "Medianaxis::ringContainsRing: NOT IMPLEMENTED outerRingid:" << outerRingId <<  "innerRingId" << innerRingId << std::endl;

    return false;
  }
  
    
  void Medianaxis::voronoiInternalEdges(jint* outerRingIds, jsize outerRingIdsLength, jint* holeRingIds, jsize holeRingIdsLength, jfloat*& outCArray, jsize& outCArrayLength, jfloat zEpsilon, jfloat dotProductLimit, jfloat resolution){
    
    std::vector<voronoi::v_segment_type> segments;
    std::vector<m_ring_type*> outerRings;
    std::vector<m_ring_type*> innerRings;
    
    const double SCALEFACTOR=1.0;
    std::cerr << "c++ voronoiInternalEdges: still using scalefactor:" <<  SCALEFACTOR << " remove it!" << std::endl;

  	for (jint i=0; i<outerRingIdsLength; i++){
      DBG std::cout << "c++ voronoiInternalEdges: outerRingid:" << outerRingIds[i] << std::endl;
      int2ptrMap_typei j = vertexMap.find(outerRingIds[i]);
      if(j != vertexMap.end()) {
        DBG std::cout << "c++ voronoiInternalEdges: outerRingid:" << outerRingIds[i] << " found" << std::endl;
        outerRings.push_back(&(j->second->ring));
        float* data = j->second->data;
        jsize size = j->second->length-2;
        for (int k=0; k<size; k+=2) {
          segments.push_back(voronoi::v_segment_type(voronoi::v_point_type(SCALEFACTOR*data[k], SCALEFACTOR*data[k+1]), voronoi::v_point_type( SCALEFACTOR*data[k+2], SCALEFACTOR*data[k+3])));
          DBG std::cout << "c++ voronoiInternalEdges: segment: (" << data[k] << "," << data[k+1] << ":" << data[k+2] << "," << data[k+3] << ")" << std::endl;
        }
      } else {
        DBG std::cout << "c++ voronoiInternalEdges: outerRingid:" << outerRingIds[i] << " not found" << std::endl;
      }
    }
    for (jint i=0; i<holeRingIdsLength; i++){
      DBG std::cout << "c++ voronoiInternalEdges: holeRingIds:" << holeRingIds[i] << std::endl;
      int2ptrMap_typei j = vertexMap.find(holeRingIds[i]);
      if(j != vertexMap.end()) {
        DBG std::cout << "c++ voronoiInternalEdges: holeRingIds:" << holeRingIds[i] << " found" << std::endl;
        innerRings.push_back(&(j->second->ring));
        
        float* data = j->second->data;
        jsize size = j->second->length-2;
        for (int k=0; k<size; k+=2) {
          segments.push_back(voronoi::v_segment_type(voronoi::v_point_type(SCALEFACTOR*data[k], SCALEFACTOR*data[k+1]), voronoi::v_point_type( SCALEFACTOR*data[k+2], SCALEFACTOR*data[k+3])));
          DBG std::cout << "c++ voronoiInternalEdges: segment: (" << data[k] << "," << data[k+1] << ":" << data[k+2] << "," << data[k+3] << ")" << std::endl;
        }
      } else {
        DBG std::cout << "c++ voronoiInternalEdges: holeRingIds:" << holeRingIds[i] << " not found" << std::endl;
      }
    }
    
    std::vector<std::vector<voronoi::v_pointz_type>*> result;
    
    voronoi::interiorVoronoiEdges(segments, result, zEpsilon, dotProductLimit, resolution);
    if (0 == result.size() ) {
      DBG std::cout << "c++ voronoiInternalEdges: No internal edges found" << std::endl;
      return;
    }
    
    for (std::vector<std::vector<voronoi::v_pointz_type>*>::iterator i=result.begin(); i<result.end(); i++){
    	for (std::vector<voronoi::v_pointz_type>::iterator j=(*i)->begin(); j<(*i)->end(); j++){
        *j = voronoi::v_pointz_type(j->x()/SCALEFACTOR, j->y()/SCALEFACTOR, j->z()/(SCALEFACTOR*SCALEFACTOR));
      }
    }
    
    std::vector<std::vector<voronoi::v_pointz_type>*> filteredResult;
    
    if (true) {
      std::vector<std::vector<voronoi::v_pointz_type>*> filteredResult0;
      //DBG
      std::cout << "c++ voronoiInternalEdges: pre filtering internal size: " << result.size() << std::endl;
   	  voronoi::filterOutInteriorVoronoiEdges(result,innerRings,filteredResult0);
      //DBG
      std::cout << "c++ voronoiInternalEdges: post filtering internal size: " << filteredResult0.size() << std::endl;
      voronoi::filterOutExteriorVoronoiEdges(filteredResult0,outerRings,filteredResult);
      //DBG
      std::cout << "c++ voronoiInternalEdges: post filtering external size: " << filteredResult.size() << std::endl;
    } else {
      filteredResult = result;
    }
    
    outCArrayLength = 0;
    for (jsize i=0 ; i<filteredResult.size(); i++) {
      if (filteredResult[i]->size() > 1){
        outCArrayLength += filteredResult[i]->size()*3 + 1;
      }
    }

    outCArray = new float[outCArrayLength];
    for (jsize i=0, k=0; i<filteredResult.size(); i++) {
      std::vector<voronoi::v_pointz_type>* edge = filteredResult[i];
      if (edge->size() > 1) {
        for (int j=0;j<edge->size();j++){
          if (k+4 > outCArrayLength) {
            std::cerr << "k: " << k << " outCArrayLength: " << outCArrayLength << std::endl;
            break;
          }
          voronoi::v_pointz_type &point = edge->at(j);
          if (isnan(point.x()) || isnan(point.y()) || isnan(point.z())) {
            std::cerr << "c++ voronoiInternalEdges: NAN found in output vertex ("<<point.x()<<","<<point.y()<<","<<point.z()<<") i,j="<<i<<","<<j<<std::endl;
          }
          outCArray[k++] = point.x();
          outCArray[k++] = point.y();
          outCArray[k++] = point.z();
          DBG std::cout << "edge: " << point.x() << ", " << point.y() << ", " << point.z() << std::endl;
        }
        DBG std::cout << "endedge:" << std::endl;
        outCArray[k++] = NAN;
      }
      delete edge;
      result[i] = NULL;
    }
  }
}