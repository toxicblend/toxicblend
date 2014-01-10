package st0le.convexHull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import toxi.geom.Vec2D;
import toxi.geom.Polygon2D;
import toxi.geom.Rect;

/**
 * A brute force O(n⁴) algorithm i found on the internetz.
 *
 * Shamelessly copied from http://stackoverflow.com/questions/10513920/convex-hull-determine-order-of-the-points.
 * @author st0le
 */
public class NaiveConvexHull {

  static private class Vec2DComp implements Comparator<Vec2D> {

    private Vec2D center;

    public Vec2DComp(Vec2D center) {
      this.center = center;
    }

    @Override
    public int compare(Vec2D o1, Vec2D o2) {
      double angle1 = Math.atan2(o1.y - center.y, o1.x - center.x);
      double angle2 = Math.atan2(o2.y - center.y, o2.x - center.x);
      if (angle1 < angle2)
        return 1;
      else if (angle1 > angle2)
        return -1;
      return 0;
    }
  }

  static public List<Vec2D> convexHull(List<Vec2D> points) {
    if (points == null)
      return Collections.emptyList();
    if (points.size() <= 3)
      return points;
    boolean[] extremeVec2Ds = new boolean[points.size()];
    Arrays.fill(extremeVec2Ds, true);
    for (int i = 0, sz = points.size(); i < sz; i++) {
      if (extremeVec2Ds[i]) {
        for (int j = 0; j < sz; j++) {
          if (i != j && extremeVec2Ds[j]) {
            for (int k = 0; k < sz; k++) {
              if (k != i && k != j) {
                for (int l = 0; l < sz; l++) {
                  if (extremeVec2Ds[l] && l != i && l != j && l != k) {
                    // Check if P[l] lies in triangle formed
                    // by
                    // P[i],P[j],P[k]

                    Polygon2D p = new Polygon2D();
                    p.add(points.get(i).x, points.get(i).y);
                    p.add(points.get(j).x, points.get(j).y);
                    p.add(points.get(k).x, points.get(k).y);
                    if (p.containsPoint(points.get(l)))
                      extremeVec2Ds[l] = false;
                  }
                }
              }
            }
          }
        }
      }
    }

    Vec2D centerOfHull = null; // Arbitrary point inside the hull
    // Order?
    for (int i = 0; i < extremeVec2Ds.length; i++) {
      if (!extremeVec2Ds[i]) {
        centerOfHull = points.get(i);
        break;
      }
    }
    Rect aabb = null;
    if (centerOfHull == null) {
      aabb = new Rect(); 
    }
    List<Vec2D> convexHull = new ArrayList<Vec2D>();
    for (int i = 0; i < extremeVec2Ds.length; i++) {
      if (extremeVec2Ds[i]) {
        Vec2D p = points.get(i);
        convexHull.add(p);
        if (centerOfHull == null) {
          aabb.growToContainPoint(p);
        }
      }
    }
    if (centerOfHull == null) {
      centerOfHull = aabb.getCentroid();
    }
    Collections.sort(convexHull, new Vec2DComp(centerOfHull));
    // or use a heap. still O(nlogn)
    return convexHull;
  }
}
