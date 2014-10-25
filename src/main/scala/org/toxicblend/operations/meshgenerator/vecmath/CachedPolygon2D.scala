package org.toxicblend.operations.meshgenerator.vecmath

import scala.collection.mutable.HashMap

/**
 * a version of Polygon2D that caches the 'containsPoint' lookups
 * Take care when using this class, you can easily run out of memory
 */
class CachedPolygon2D(vertices:IndexedSeq[Vec2D]) extends Polygon2D(vertices) {
  val hashmap = new HashMap[Vec2D, Boolean]
  
  override def containsPoint(p:Vec2D):Boolean = {
    val cache = hashmap.get(p)
    if (cache.isDefined) cache.get
    else {
      val sample = super.containsPoint(p)
      hashmap.put(p,sample)
      sample
    }
  }
}