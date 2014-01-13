//
//  voronoiedges.h
//  VoronoiEdges
//
//  Created by Daniel on 2013-05-18.
//  Copyright (c) 2013 None. All rights reserved.
//

#ifndef medianaxis_voronoiedges_h
#define medianaxis_voronoiedges_h
#include "medianaxis.hpp"
#include <jni.h>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/polygon/segment_data.hpp>
#include "pointz_data.hpp"

namespace medianAxisFloat {
  namespace voronoi {
    
    typedef medianAxisFloat::m_coordinate_type v_coordinate_type;
    typedef rectangle_data<v_coordinate_type> v_rect_type;
    typedef boost::polygon::point_data<v_coordinate_type> v_point_type;
    typedef boost::polygon::segment_data<v_coordinate_type> v_segment_type;
    typedef boost::geometry::model::d2::point_xy<v_coordinate_type> v_point_xy;
    typedef pointz_data<v_coordinate_type> v_pointz_type;
    typedef boost::geometry::strategy::transform::ublas_transformer<v_coordinate_type, 2, 2> v_transform_type;

    void interiorVoronoiEdges(const std::vector<v_segment_type>& inSegments, std::vector<std::vector<v_pointz_type>*>& result, v_coordinate_type zEpsilon, v_coordinate_type dotProductLimit, double resolution);
    void filterOutInteriorVoronoiEdges(const std::vector<std::vector<v_pointz_type>*>& input, const std::vector<m_ring_type*>& innerRings, std::vector<std::vector<v_pointz_type>*>& result);
    void filterOutExteriorVoronoiEdges(const std::vector<std::vector<v_pointz_type>*>& input, const std::vector<m_ring_type*>& outerRings, std::vector<std::vector<v_pointz_type>*>& result);
  }
}

// register medianAxisFloat::voronoi::v_point_xy as a point with boost polygon
namespace boost { namespace polygon {
  template <>
  struct geometry_concept<medianAxisFloat::voronoi::v_point_xy> { typedef point_concept type; };
  
  //Then we specialize the gtl point traits for our point type
  template <>
  struct point_traits<medianAxisFloat::voronoi::v_point_xy> {
    typedef int coordinate_type;
    
    static inline medianAxisFloat::voronoi::v_coordinate_type get(const medianAxisFloat::voronoi::v_point_xy& point,
                                      orientation_2d orient) {
      if(orient == HORIZONTAL)
        return point.x();
      return point.y();
    }
  };
  
  template <>
  struct point_mutable_traits<medianAxisFloat::voronoi::v_point_xy> {
    typedef int coordinate_type;
    
    static inline void set(medianAxisFloat::voronoi::v_point_xy& point, orientation_2d orient, int value) {
      if(orient == HORIZONTAL)
        point.x(value);
      else
        point.y(value);
    }
    static inline medianAxisFloat::voronoi::v_point_xy construct(int x_value, int y_value) {
      medianAxisFloat::voronoi::v_point_xy retval;
      retval.x(x_value);
      retval.y(y_value);
      return retval;
    }
  };
} }
#endif
