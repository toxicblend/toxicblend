//
//  pointz_data.h
//  VoronoiVisualizer
//
//  Created by Daniel on 2013-03-29.
//  Copyright (c) 2013 Daniel. All rights reserved.
//

#ifndef VoronoiVisualizer_pointz_data_h
#define VoronoiVisualizer_pointz_data_h

#include <boost/polygon/point_data.hpp>
#include <boost/polygon/isotropy.hpp>
#include <boost/polygon/point_concept.hpp>

using namespace boost::polygon;
namespace medianAxisFloat {
  
  template <typename T>
  class pointz_data : public point_data<T> {
  public:
    //typedef T coordinate_type;
    
    pointz_data() : point_data<T>()
    {}
    
    pointz_data(T x, T y, T z) :point_data<T>(x,y){
      coordZ = z;
    };
    
    pointz_data(T x, T y):point_data<T>(x,y) {
      coordZ = 0;
    }
    
    pointz_data(const point_data<T>& that):point_data<T>(that){
      coordZ = 0;
    }
    
    explicit pointz_data(const pointz_data& that):point_data<T>(that){
      coordZ = that.coordZ;
    }
    
    pointz_data& operator=(const pointz_data& that) {
      point_data<T>::operator=(that);
      coordZ = that.coordZ;
      return *this;
    }
    
    template <typename PointType>
    explicit pointz_data(const PointType& that) {
      *this = that;
    }
    
    template <typename PointType>
    pointz_data& operator=(const PointType& that) {
      assign(*this, that);
      return *this;
    }
      
    T z() const {
      return coordZ;
    }
    
    pointz_data& z(T value) {
      coordZ = value;
      return *this;
    }
    
    bool operator==(const pointz_data& that) const {
      return (point_data<T>::operator==(that) &&
              (coordZ == that.coordZ));
    }
    
    bool operator!=(const pointz_data& that) const {
      return !(*this == that);
    }
    
    bool operator<=(const pointz_data& that) const {
      return !(that < *this);
    }
    
    bool operator>(const pointz_data& that) const {
      return that < *this;
    }
    
    bool operator>=(const pointz_data& that) const {
      return !(*this < that);
    }
    
  private:
    T coordZ;
  };
}
namespace boost {
  namespace polygon {
    template <typename CType>
    struct geometry_concept< medianAxisFloat::pointz_data<CType> > {
      typedef point_concept type;
    };
  }
}

#endif
