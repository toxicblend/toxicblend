// Boost.Polygon library voronoi_visualizer.cpp file

//          Copyright Andrii Sydorchuk 2010-2012.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// See http://www.boost.org for updates, documentation, and revision history.


#include <math.h>
#include "voronoiedges.hpp"

#include <iostream>
#include <vector>
#include <boost/ptr_container/ptr_list.hpp>
#include <boost/polygon/polygon.hpp>
#include <boost/polygon/voronoi.hpp>
#include <boost/geometry/algorithms/distance.hpp>
#include <boost/geometry/geometry.hpp>
#include <boost/geometry/geometries/linestring.hpp>

#include <boost/geometry/algorithms/comparable_distance.hpp>

//using namespace boost::polygon;
using std::string;

#include "voronoi_visual_utils.hpp"
#include "pointz_data.hpp"

// register m_point_type as a point with boost polygon
namespace boost { namespace polygon {
  template <>
  struct geometry_concept<medianAxisFloat::voronoi::v_pointz_type> { typedef point_concept type; };
  //Then we specialize the gtl point traits for our point type
  template <>
  struct point_traits<medianAxisFloat::voronoi::v_pointz_type> {
    typedef int coordinate_type;
    static inline coordinate_type get(const medianAxisFloat::voronoi::v_pointz_type& point,
                                      orientation_2d orient) {
      if(orient == HORIZONTAL)
        return point.x();
      return point.y();
    }
  };
  template <>
  struct point_mutable_traits<medianAxisFloat::voronoi::v_pointz_type> {
    typedef int coordinate_type;
    static inline void set(medianAxisFloat::voronoi::v_pointz_type& point, orientation_2d orient, int value) {
      if(orient == HORIZONTAL)
        point.x(value);
      else
        point.y(value);
    }
    static inline medianAxisFloat::voronoi::v_pointz_type construct(int x_value, int y_value) {
      medianAxisFloat::voronoi::v_pointz_type retval;
      retval.x(x_value);
      retval.y(y_value);
      return retval;
    }
  };
} }

namespace medianAxisFloat {
  namespace voronoi {
    
    class Voronoi {
      
    public:
      
      void build(const std::vector<v_segment_type>& segments) {
        // Clear all containers.
        clear();
        
        // Read data.
        read_data(segments);
        
        // No data, don't proceed.
        if (!brect_initialized_) {
          return;
        }
        
        // Construct bounding rectangle.
        construct_brect();
        
        // Construct voronoi diagram.
        construct_voronoi(
                          //point_data_.begin(), point_data_.end(),
                          segment_data_.begin(), segment_data_.end(),
                          &vd_);
        
        // Color exterior edges.
        for (const_edge_iterator it = vd_.edges().begin();
             it != vd_.edges().end(); ++it) {
          if (!it->is_finite()) {
            color_exterior(&(*it));
          }
        }
      }
      
    protected:
      
      typedef voronoi_builder<int> VB;
      typedef voronoi_diagram<v_coordinate_type> VD;
      typedef VD::cell_type cell_type;
      typedef VD::cell_type::source_index_type source_index_type;
      typedef VD::cell_type::source_category_type source_category_type;
      typedef VD::edge_type edge_type;
      typedef VD::cell_container_type cell_container_type;
      typedef VD::cell_container_type vertex_container_type;
      typedef VD::edge_container_type edge_container_type;
      typedef VD::const_cell_iterator const_cell_iterator;
      typedef VD::const_vertex_iterator const_vertex_iterator;
      typedef VD::const_edge_iterator const_edge_iterator;
    
    private:
      static const std::size_t EXTERNAL_COLOR = 1;
      
      v_point_type shift_;
      std::vector<v_point_type> point_data_;
      std::vector<v_segment_type> segment_data_;
      v_rect_type brect_;
      VB vb_;
      VD vd_;
      bool brect_initialized_;
      
      void clear() {
        brect_initialized_ = false;
        point_data_.clear();
        segment_data_.clear();
        vd_.clear();
      }
      
      void read_data(const std::vector<v_segment_type>& segments) {
        std::vector<v_point_type> points;
        //points.push_back(point_type(0, 0));
        //points.push_back(point_type(1, 6));
        
        std::size_t num_points, num_segments;
        num_points = points.size();
        
        for (std::size_t i = 0; i < num_points; ++i) {
          update_brect(points[i]);
          point_data_.push_back(points[i]);
        }
        num_segments = segments.size();
        for (std::size_t i = 0; i < num_segments; ++i) {
          update_brect(segments[i].low());
          update_brect(segments[i].high());
          segment_data_.push_back(segments[i]);
        }
      }
      
      void update_brect(const v_point_type& point) {
        if (brect_initialized_) {
          encompass(brect_, point);
        } else {
          set_points(brect_, point, point);
          brect_initialized_ = true;
        }
      }
      
      void construct_brect() {
        double side = (std::max)(xh(brect_) - xl(brect_), yh(brect_) - yl(brect_));
        center(shift_, brect_);
        set_points(brect_, shift_, shift_);
        bloat(brect_, side * 1.2);
      }
      
      void color_exterior(const VD::edge_type* edge) {
        if (edge->color() == EXTERNAL_COLOR) {
          return;
        }
        edge->color(EXTERNAL_COLOR);
        edge->twin()->color(EXTERNAL_COLOR);
        const VD::vertex_type* v = edge->vertex1();
        if (v == NULL || !edge->is_primary()) {
          return;
        }
        v->color(EXTERNAL_COLOR);
        const VD::edge_type* e = v->incident_edge();
        do {
          color_exterior(e);
          e = e->rot_next();
        } while (e != v->incident_edge());
      }
      
    public:
      void sample_edges(std::vector<std::vector<v_pointz_type>*> & result, v_coordinate_type zEpsilon, v_coordinate_type dotProductLimit) {
        result.clear();
        for (const_edge_iterator it = vd_.edges().begin();
             it != vd_.edges().end(); ++it) {
          if (!it->is_primary()) {
            continue;
          }
          if ((it->color() == EXTERNAL_COLOR)) {
            continue;
          }
          std::vector<v_pointz_type>* sampleZ = new std::vector<v_pointz_type>;
          {
            std::vector<v_point_type> samples;
            if (!it->is_finite()) {
              clip_infinite_edge(*it, &samples);
            } else {
              v_point_type vertex0(it->vertex0()->x(), it->vertex0()->y());
              v_point_type vertex1(it->vertex1()->x(), it->vertex1()->y());
              if (it->is_curved()) {
                samples.push_back(vertex0);
                samples.push_back(vertex1);
                sample_curved_edge(*it, &samples);
              } else {
                sample_linear_edge(vertex0, vertex1, &samples);
              }
            }
            calculate_edge_distance_and_filter(*it,&samples,sampleZ, zEpsilon, dotProductLimit);
          }
          if (sampleZ->size()>1){
            result.push_back(sampleZ);
          } else {
            delete sampleZ;
          }
        }
      }
      
    private:
      void clip_infinite_edge( const edge_type& edge, std::vector<v_point_type>* clipped_edge) {
        const cell_type& cell1 = *edge.cell();
        const cell_type& cell2 = *edge.twin()->cell();
        v_point_type origin, direction;
        // Infinite edges could not be created by two segment sites.
        if (cell1.contains_point() && cell2.contains_point()) {
          v_point_type p1 = retrieve_point(cell1);
          v_point_type p2 = retrieve_point(cell2);
          origin.x((p1.x() + p2.x()) * 0.5);
          origin.y((p1.y() + p2.y()) * 0.5);
          direction.x(p1.y() - p2.y());
          direction.y(p2.x() - p1.x());
        } else {
          origin = cell1.contains_segment() ?
          retrieve_point(cell2) :
          retrieve_point(cell1);
          v_segment_type segment = cell1.contains_segment() ?
          retrieve_segment(cell1) :
          retrieve_segment(cell2);
          v_coordinate_type dx = high(segment).x() - low(segment).x();
          v_coordinate_type dy = high(segment).y() - low(segment).y();
          if ((low(segment) == origin) ^ cell1.contains_point()) {
            direction.x(dy);
            direction.y(-dx);
          } else {
            direction.x(-dy);
            direction.y(dx);
          }
        }
        v_coordinate_type side = xh(brect_) - xl(brect_);
        v_coordinate_type koef =
        side / (std::max)(fabs(direction.x()), fabs(direction.y()));
        if (edge.vertex0() == NULL) {
          clipped_edge->push_back(v_point_type(
                                               origin.x() - direction.x() * koef,
                                               origin.y() - direction.y() * koef));
        } else {
          clipped_edge->push_back(v_point_type(edge.vertex0()->x(), edge.vertex0()->y()));
        }
        if (edge.vertex1() == NULL) {
          clipped_edge->push_back(v_point_type(
                                               origin.x() + direction.x() * koef,
                                               origin.y() + direction.y() * koef));
        } else {
          clipped_edge->push_back(v_point_type(edge.vertex1()->x(), edge.vertex1()->y()));
        }
      }
      
      void calculate_edge_distance_and_filter( const edge_type& edge,
                                               const std::vector<v_point_type>* sampled_edge,
                                               std::vector<v_pointz_type>* resulting_edge,
                                               v_coordinate_type zEpsilon,
                                               v_coordinate_type dotProductLimit) {
                
        std::vector<v_pointz_type> z_edge;
        bool noZeroDistanceFound = true;
        v_point_type point = edge.cell()->contains_point() ?
        retrieve_point(*edge.cell()) :
        retrieve_point(*edge.twin()->cell());
        v_segment_type segment = edge.cell()->contains_point() ?
        retrieve_segment(*edge.twin()->cell()) :
        retrieve_segment(*edge.cell());
        for (std::size_t i = 0; noZeroDistanceFound && i < sampled_edge->size(); ++i) {
                
          const v_point_type& point = sampled_edge->at(i);
          const medianAxisFloat::m_point_type p0(point.x(), point.y());
          
          boost::geometry::model::linestring<medianAxisFloat::m_point_type> linestring;
          const medianAxisFloat::m_point_type p1(segment.low().x(),segment.low().y());
          const medianAxisFloat::m_point_type p2(segment.high().x(),segment.high().y());

          linestring.push_back(p1);
          linestring.push_back(p2);
          
          // distances from geometry to voronoi edges can only be positive
          v_coordinate_type distance = fabs(boost::geometry::comparable_distance(p0, linestring));
          {
            v_coordinate_type distance2 = fabs(boost::geometry::comparable_distance(p0, p1));
            if (distance2 < distance){
              distance = distance2;
              std::cout << "point distance was shorter" << std::endl;
            }
            distance2 = fabs(boost::geometry::comparable_distance(p0, p2));
            if (distance2 < distance){
              distance = distance2;
              std::cout << "point distance was shorter" << std::endl;
            }
          }
          distance = sqrt(distance);
          //euclidean_distance(segment,point);
          if (distance <= zEpsilon) {
            noZeroDistanceFound = false;
          }
          
          v_pointz_type z(point.x(), point.y(), distance);
          z_edge.push_back(z);
        }
        if (noZeroDistanceFound) {
          resulting_edge->insert( resulting_edge->end(), z_edge.begin(), z_edge.end() );
        } else {
          for (size_t i=0; i<z_edge.size(); i++){
            // find the zero distance segment again
            const v_coordinate_type& distance = z_edge[i].z();
            if (distance <= zEpsilon) {
              // the point with zero distance to the enclosing ring
              const v_point_type* edge_point1 = NULL;
              // the other point for the sampled edge
              const v_point_type* edge_point2 = NULL;

              if (i==0) {
                edge_point1 = &sampled_edge->at(i);
                edge_point2 = &sampled_edge->at(i+1);
              } else if (i==z_edge.size()-1){
                edge_point1 = &sampled_edge->at(i);
                edge_point2 = &sampled_edge->at(i-1);
              } else {
                std::cerr << "calculate_edge_distance_and_filter:: logic is flawed" << std::endl;
              }
              
              if (NULL!=edge_point1 && NULL != edge_point2){
                medianAxisFloat::m_point_type vRing(segment.low().x(),segment.low().y());
                boost::geometry::subtract_point(vRing, medianAxisFloat::m_point_type(segment.high().x(),segment.high().y()));
                boost::geometry::divide_value(vRing, sqrt(vRing.x()*vRing.x()+vRing.y()*vRing.y()) );

                
                medianAxisFloat::m_point_type vEdge(edge_point1->x(), edge_point1->y());
                boost::geometry::subtract_point(vEdge, medianAxisFloat::m_point_type(edge_point2->x(), edge_point2->y()));
                boost::geometry::divide_value(vEdge, sqrt(vEdge.x()*vEdge.x()+vEdge.y()*vEdge.y()) );

                v_coordinate_type dotProduct = boost::geometry::dot_product(vRing, vEdge);
                if (dotProduct < dotProductLimit && dotProduct > -dotProductLimit) {
                  DBG std::cout << "Deleting edge with dotProduct:" << dotProduct << " and z distance:" << distance << " zEpsilon:" << zEpsilon <<  std::endl;
                  /*const v_point_type* opposite_edge_point = NULL;

                  if (i==0) {
                    opposite_edge_point = &sampled_edge->at(z_edge.size()-1);
                  } else if (i==z_edge.size()-1){
                    opposite_edge_point = &sampled_edge->at(0);
                  } else {
                    std::cerr << "calculate_edge_distance_and_filter:: locic is still flawed" << std::endl;
                  }
                  // Still saving the opposite point of this zero distance edge, or else the curve will be broken up
                  //resulting_edge->push_back(*opposite_edge_point);
                  */
                  break;
    
                } else {
                  DBG std::cout << "Keeping edge with dotProduct:" << dotProduct << " and z distance:" << distance << " zEpsilon:" << zEpsilon <<std::endl;
                  resulting_edge->insert( resulting_edge->end(), z_edge.begin(), z_edge.end() );
                  break;
                }
              } else {
                std::cerr << "calculate_edge_distance_and_filter:: could not (re)find zero point of z distance:" << distance << " zEpsilon:" << zEpsilon <<std::endl;
              }
            }
          }
        }
      }
      
      void sample_curved_edge( const edge_type& edge,
                              std::vector<v_point_type>* sampled_edge) {
        v_coordinate_type max_dist = 1E-3 * (xh(brect_) - xl(brect_));
        v_point_type point = edge.cell()->contains_point() ?
        retrieve_point(*edge.cell()) :
        retrieve_point(*edge.twin()->cell());
        v_segment_type segment = edge.cell()->contains_point() ?
        retrieve_segment(*edge.twin()->cell()) :
        retrieve_segment(*edge.cell());
        voronoi_visual_utils<v_coordinate_type>::discretize( point, segment, max_dist, sampled_edge);
      }
      
      void sample_linear_edge( const v_point_type& v0, const v_point_type & v1,
                              std::vector<v_point_type>* sampled_edge) {
        //coordinate_type max_dist = 1E-3 * (xh(brect_) - xl(brect_));
        sampled_edge->push_back(v0);
        sampled_edge->push_back(v1);
      }
      
      v_point_type retrieve_point(const cell_type& cell) {
        source_index_type index = cell.source_index();
        source_category_type category = cell.source_category();
        if (category == SOURCE_CATEGORY_SINGLE_POINT) {
          return point_data_[index];
        }
        index -= point_data_.size();
        if (category == SOURCE_CATEGORY_SEGMENT_START_POINT) {
          return low(segment_data_[index]);
        } else {
          return high(segment_data_[index]);
        }
      }
      
      v_segment_type retrieve_segment(const cell_type& cell) {
        source_index_type index = cell.source_index() - point_data_.size();
        return segment_data_[index];
      }
    };  // end class Voronoi
    
    /**
     * Make an affine transform that will transform from the cordinate system defined by aabbFrom
     * to the coordinate system of aabbTo.
     * e.g. when the point in the lower left corner of aabbFrom is transformed, it will be in
     * the lower left corner of aabbTo
     *
     * @param aabbFrom  the bounding box we convert from
     * @param aabbTo  the bounding box we convert to
     * @param scale  a custom scale, if set to NAN a calculated value will be used
     */
    v_transform_type makeTransform(const v_rect_type& aabbFrom, const v_rect_type& aabbTo, v_coordinate_type scale) {
      boost::numeric::ublas::matrix<m_coordinate_type> matrix(3,3);matrix.clear();
      // from values
      v_coordinate_type fx = xl(aabbFrom); //.get(boost::polygon::HORIZONTAL).low();
      v_coordinate_type fw = xh(aabbFrom)-fx; // .get(boost::polygon::HORIZONTAL).high()-fx;
      v_coordinate_type fxc = fx+fw/2.0;
      v_coordinate_type fy = yl(aabbFrom); // .get(boost::polygon::VERTICAL).low();
      v_coordinate_type fh = yh(aabbFrom)-fy;//.get(boost::polygon::VERTICAL).high()-fy;
      v_coordinate_type fyc = fy+fh/2.0;
      // to values
      v_coordinate_type tx = xl(aabbTo);//.get(boost::polygon::HORIZONTAL).low();
      v_coordinate_type tw = xh(aabbTo)-tx;//.get(boost::polygon::HORIZONTAL).high()-tx;
      v_coordinate_type txc = tx+tw/2.0;
      v_coordinate_type ty = yl(aabbTo);//.get(boost::polygon::VERTICAL).low();
      v_coordinate_type th = yh(aabbTo)-ty;//.get(boost::polygon::VERTICAL).high()-ty;
      v_coordinate_type tyc = ty+th/2.0;
      
      if (isnan(scale)) {
        scale = std::min<m_coordinate_type>(tw/fw, th/fh);
      }
      matrix(0,0) = scale;
      matrix(1,1) = scale;
      matrix(2,2) = 1.0;
      matrix(0,2) = txc-fxc*scale;
      matrix(1,2) = tyc-fyc*scale;
      v_transform_type transform(matrix);
      return transform;
    }
    
    void interiorVoronoiEdges(const std::vector<v_segment_type>& inSegments, std::vector<std::vector<v_pointz_type>*>& result,
                              v_coordinate_type zEpsilon, v_coordinate_type dotProductLimit, double resolution){
      
      v_rect_type fromAabb(inSegments.at(0).low().x(),inSegments.at(0).low().y(),inSegments.at(0).high().x(),inSegments.at(0).high().y());
      v_rect_type toAabb(-resolution,-resolution,resolution,resolution);
      for (std::vector<v_segment_type>::const_iterator it=inSegments.begin(); it<inSegments.end(); it++){
        v_point_xy lp(it->low().x(),it->low().y());
        v_point_xy hp(it->low().x(),it->low().y());
        encompass(fromAabb,lp);
        encompass(fromAabb,hp);
      }
      v_transform_type transform = makeTransform(fromAabb,toAabb, NAN);
      v_coordinate_type scale = transform.matrix()(0,0);
      v_transform_type inverseTransform = makeTransform(toAabb,fromAabb,1.0/scale);
      
      std::cout << "fromAabb: ("<<xl(fromAabb)<<","<<yl(fromAabb)<<")("<<xh(fromAabb)<<","<<yh(fromAabb)<<")"<< std::endl;
      std::cout << "  toAabb: ("<<xl(toAabb)<<","<<yl(toAabb)<<")("<<xh(toAabb)<<","<<yh(toAabb)<<")"<< std::endl;
      std::cout << "transform: "<< transform.matrix() << " scale: " << scale << std::endl;
      // we are enlarging the whole geometry, including the epsilon condition
      //zEpsilon = zEpsilon*scale;
      
      std::vector<v_segment_type> segments;
      for (std::vector<v_segment_type>::const_iterator it=inSegments.begin(); it<inSegments.end(); it++){
        // i can't get v_pointz_type to work with boost::geometry::transform so i make the detour via point_xy
        v_point_xy a(it->low().x(),it->low().y());
        v_point_xy b(it->high().x(),it->high().y());
        boost::geometry::transform(a,a,transform);
        boost::geometry::transform(b,b,transform);
        if (isnan(a.x())||isnan(a.y())||isnan(b.x())||isnan(b.y()))
        {
          std::cerr << "c++ interiorVoronoiEdges found NAN in input segment ("<<a.x()<<","<<a.y()<<")("<<b.x()<<","<<b.y()<<")"<< std::endl;
        } else {
          v_point_type aa(a.x(),a.y());
          v_point_type bb(b.x(),b.y());
          v_segment_type dst(aa,bb);
          segments.push_back(dst);
        }
      }
      
      Voronoi vo;
      vo.build(segments);
      vo.sample_edges(result, zEpsilon, dotProductLimit);
      
      for (std::vector<std::vector<voronoi::v_pointz_type>*>::iterator i=result.begin(); i<result.end(); i++){
        if ((*i)->size() < 2) {
          std::cerr << "c++ interiorVoronoiEdges found almost empty edge. vector size=" <<(*i)->size() << std::endl;
        }
        for (std::vector<voronoi::v_pointz_type>::iterator j=(*i)->begin(); j<(*i)->end(); j++){
          if (isnan(j->x())||isnan(j->y())||isnan(j->z())){
            std::cerr << "c++ interiorVoronoiEdges found NAN in output point ("<<j->x()<<","<<j->y()<<","<<j->z()<<")"<< std::endl;
          }
          // i can't get v_pointz_type to work with boost::geometry::transform so i make the detour via point_xy
          v_point_xy p(j->x(),j->y());
          boost::geometry::transform(p,p,inverseTransform);
          if (j->z()<0.0){
            std::cerr << "c++ interiorVoronoiEdges found negative z component in output point ("<<p.x()<<","<<p.y()<<","<<j->z()/scale<<")"<<std::endl;
          }
          if (isnan(p.x())||isnan(p.y())||isnan(j->z()/scale)){
            std::cerr << "c++ interiorVoronoiEdges found NAN in transformed output point ("<<p.x()<<","<<p.y()<<","<<j->z()/scale<<")"<<std::endl;
          }
          *j = voronoi::v_pointz_type(p.x(), p.y(), j->z()/scale);
        }
      }
    }
    
    void filterOutInteriorVoronoiEdges(const std::vector<std::vector<v_pointz_type>*>& input, const std::vector<medianAxisFloat::m_ring_type*>& innerRings, std::vector<std::vector<v_pointz_type>*>& result){
      for (std::vector<std::vector<v_pointz_type>*>::const_iterator i=input.begin(); i<input.end(); i++){
        size_t edgeSize = (*i)->size();
        //std::cout << "filterOutInteriorVoronoiEdges: loop 1 edgeSize(" << edgeSize << ")" << std::endl;
        
        bool insideInner = false;
        
        for(size_t j=1; j<edgeSize; j++){
          //std::cout << "filterOutInteriorVoronoiEdges: loop 2"<< std::endl;
          
          v_pointz_type& lastP = (*i)->at(j-1);
          v_pointz_type& currP = (*i)->at(j);
          const m_point_type median((currP.x()+lastP.x())/2.0, (currP.y()+lastP.y())/2.0);
          //std::cout << "filterOutInteriorVoronoiEdges: testing median (" << median.x() << "," << median.y() << ")" << std::endl;
          
          
          for(size_t k=0; k<innerRings.size(); k++) {
            //std::cout << "filterOutInteriorVoronoiEdges: loop 3"<< std::endl;
            
            if(boost::geometry::within(median,*(innerRings[k]))) {
              DBG std::cout << "filterOutInteriorVoronoiEdges: median found inside inner loop (" << median.x() << "," << median.y() << ")" << std::endl;
              insideInner = true;
              break;
            }
          }
          if(insideInner) break;
        }
        if (!insideInner) {
          result.push_back((*i));
        }
      }
    }
    
    void filterOutExteriorVoronoiEdges(const std::vector<std::vector<v_pointz_type>*>& input, const std::vector<m_ring_type*>& innerRings, std::vector<std::vector<v_pointz_type>*>& result){
      for (std::vector<std::vector<v_pointz_type>*>::const_iterator i=input.begin(); i<input.end(); i++){
        size_t edgeSize = (*i)->size();
        //std::cout << "filterOutExteriorVoronoiEdges: loop 1 edgeSize(" << edgeSize << ")" << std::endl;
        
        bool outsideOuter = false;
        
        for(size_t j=1; j<edgeSize; j++){
          //std::cout << "filterOutExteriorVoronoiEdges: loop 2"<< std::endl;
          
          v_pointz_type& lastP = (*i)->at(j-1);
          v_pointz_type& currP = (*i)->at(j);
          const m_point_type median((currP.x()+lastP.x())/2.0, (currP.y()+lastP.y())/2.0);
          //std::cout << "filterOutExteriorVoronoiEdges: testing median (" << median.x() << "," << median.y() << ")" << std::endl;
          
          
          for(size_t k=0; k<innerRings.size(); k++) {
            //std::cout << "filterOutExteriorVoronoiEdges: loop 3"<< std::endl;
            
            if(!boost::geometry::within(median,*(innerRings[k])) ) {
              DBG std::cout << "filterOutExteriorVoronoiEdges: median found outside outer loop (" << median.x() << "," << median.y() << ")" << std::endl;
              outsideOuter = true;
              break;
            }
          }
          if(outsideOuter) break;
        }
        if (!outsideOuter) {
          result.push_back((*i));
        }
      }
    }
  }
}