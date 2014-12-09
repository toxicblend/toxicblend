package org.toxicblend.vecmath

trait Vec2D {
  def x:Double
  def y:Double
  def interpolateTo(v:Vec2D): Vec2D
  def scale(s:Double): Vec2D
  def add(xp:Double, yp:Double): Vec2D
  def add(v:Vec2D): Vec2D
  def sub(xp:Double, yp:Double): Vec2D
  def sub(v:Vec2D): Vec2D
  /**
   * almost equal operator
   */
  def =~=(v:Vec2D, ε:Double): Boolean
  def normalized:Vec2D
  @inline final def magnitude:Double = Vec2D.magnitude(x,y)
  @inline final def magnitudeSquared:Double = Vec2D.magnitudeSquared(x,y)
  @inline final def distanceTo(vx:Double, vy:Double) = math.sqrt(Vec2D.distanceToSquared(x,y,vx,vy))
  @inline final def distanceTo(v:Vec2D) = math.sqrt(distanceToSquared(v))
  @inline final def distanceToSquared(v:Vec2D):Double = distanceToSquared(v.x, v.y)
  @inline final def distanceToSquared(vx:Double, vy:Double) = Vec2D.distanceToSquared(x,y,vx,vy)
  @inline final def heading = math.atan2(y, x)
  def interpolateTo(v:Vec2D, f:Double): Vec2D
  def toIntString:String
  def canEqual(other: Any):Boolean
  def copy:Vec2D
  @inline final def cross(v:Vec2D) = Vec2D.cross(x,y,v.x,v.y)
  @inline final def dot(v:Vec2D) = Vec2D.dot(x,y,v.x,v.y)
}

object Vec2D {
  def apply(x:Int, y:Int) = new ImmutableVec2D(x, y)
  def apply(x:Double, y:Double) = new ImmutableVec2D(x, y)
  def apply() = new ImmutableVec2D(0d, 0d)
  def apply(angle:Double) = new ImmutableVec2D(angle)
  
  @inline def almostEqual(v1:Vec2D, v2:Vec2D, ε:Double) = (v1.eq(v2)) || ( (v1.x - v2.x).abs < ε && (v1.y - v2.y).abs < ε)
  
  @inline final def distanceToSquared(p1x:Double, p1y:Double, p2x:Double, p2y:Double) = {
    val dx = p1x - p2x
    val dy = p1y - p2y
    dx*dx + dy*dy
  }
  
  @inline final def magnitudeSquared(x:Double, y:Double) = x*x + y*y
  @inline final def magnitude(x:Double, y:Double) = math.sqrt(magnitudeSquared(x,y))
  
  /**
   * Dot product
   */
  @inline final def dot(v1x:Double, v1y:Double, v2x:Double, v2y:Double) = (v1x * v2x) + (v1y * v2y)
  
  /**
   * returns +1 if (o->a) and (o->b) is at a counterclockwise angle
   * -1 if (o->a) and (o->b) is at a clockwise angle
   * and 0 (o->a) and (o->b) are collinear
   */
  @inline final def ccw(o:Vec2D, a:Vec2D, b:Vec2D) = (a.x - o.x) * (b.y - o.y) - (b.x - o.x) * (a.y - o.y)
  
  /**
   * returns the angle between a->b->c
   */
  @inline final def angle(a:Vec2D, b:Vec2D, c:Vec2D) = math.acos(math.sqrt(normalizedDotSquared(b,a,c)))
  
  /**
   * cross product
   */
  @inline final def cross(v1x:Double, v1y:Double, v2x:Double, v2y:Double ) = (v1x * v2y) - (v1y * v2x)
  
  /**
   * cross product between (o->a) and (o->b) 
   */
  @inline final def cross(o:Vec2D, a:Vec2D, b:Vec2D) = (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)
  
  /**
   * dot product between (o->a) and (o->b) 
   */
  @inline final def dot(o:Vec2D, a:Vec2D, b:Vec2D) = (a.x - o.x) * (b.x - o.x) + (a.y - o.y) * (b.y - o.y)
  
  /**
   * normalized dot product between (o->a) and (o->b) 
   * == oa.dot(ob) * oa.dot(ob) / (oa.magnitudeSquared*ob.magnitudeSquared) == cos(angle)*cos(angle)
   */
  @inline final def normalizedDotSquared(o:Vec2D, a:Vec2D, b:Vec2D) = {
    val oax = (a.x - o.x)
    val oay = (a.y - o.y) 
    val obx = (b.x - o.x)
    val oby = (b.y - o.y)
    val n = (oax*obx + oay*oby)
    n*n/( (oax*oax + oay*oay)*(obx*obx + oby*oby))
  }
  
  /**
   * same as normalizedDotSquaredWithSign, but with "sign(cos(angle))" retained.
   * The (negative) output of this method sorts in the same way as the angle between the vectors 
   * (but without any trigometrical method calls)
   * 
   * graph: http://www.wolframalpha.com/share/clip?f=d41d8cd98f00b204e9800998ecf8427en8fh10hqa0
   * 
   * Useful when you need to sort vector pairs by the angle between them.
   * 
   * Table:
   * αDeg=0   -normalizedDotSquaredWithSign(o,a,b)=-1
   * αDeg=15  -normalizedDotSquaredWithSign(o,a,b)=-0.9330127018922192 
   * αDeg=30  -normalizedDotSquaredWithSign(o,a,b)=-0.75 
   * αDeg=45  -normalizedDotSquaredWithSign(o,a,b)=-0.4999999999999999 
   * αDeg=60  -normalizedDotSquaredWithSign(o,a,b)=-0.25
   * αDeg=75  -normalizedDotSquaredWithSign(o,a,b)=-0.06698729810778066
   * αDeg=90  -normalizedDotSquaredWithSign(o,a,b)=-0.0
   * αDeg=105 -normalizedDotSquaredWithSign(o,a,b)=0.06698729810778073
   * αDeg=120 -normalizedDotSquaredWithSign(o,a,b)=0.2499999999999998
   * αDeg=135 -normalizedDotSquaredWithSign(o,a,b)=0.4999999999999999
   * αDeg=150 -normalizedDotSquaredWithSign(o,a,b)=0.75
   * αDeg=165 -normalizedDotSquaredWithSign(o,a,b)=0.9330127018922192
   * αDeg=180 -normalizedDotSquaredWithSign(o,a,b)=1.0000000000000002
   * αDeg=195 -normalizedDotSquaredWithSign(o,a,b)=0.9330127018922194
   * αDeg=210 -normalizedDotSquaredWithSign(o,a,b)=0.7499999999999999
   * αDeg=225 -normalizedDotSquaredWithSign(o,a,b)=0.5000000000000001
   * αDeg=240 -normalizedDotSquaredWithSign(o,a,b)=0.25000000000000044
   * αDeg=255 -normalizedDotSquaredWithSign(o,a,b)=0.06698729810778059
   * αDeg=270 -normalizedDotSquaredWithSign(o,a,b)=2.1912802922805885E-32
   * αDeg=285 -normalizedDotSquaredWithSign(o,a,b)=-0.06698729810778091
   * αDeg=300 -normalizedDotSquaredWithSign(o,a,b)=-0.25000000000000017 
   * αDeg=315 -normalizedDotSquaredWithSign(o,a,b)=-0.4999999999999999
   * αDeg=330 -normalizedDotSquaredWithSign(o,a,b)=-0.7499999999999993
   * αDeg=345 -normalizedDotSquaredWithSign(o,a,b)=-0.9330127018922192
   * αDeg=360 -normalizedDotSquaredWithSign(o,a,b)=-1.0000000000000002
   * 
   * (maximum angle between vectors is 180 degrees)
   */
  @inline final def normalizedDotSquaredWithSign(o:Vec2D, a:Vec2D, b:Vec2D) = {
    val oax = (a.x - o.x)
    val oay = (a.y - o.y) 
    val obx = (b.x - o.x)
    val oby = (b.y - o.y)
    val n = (oax*obx + oay*oby)
    if (n < 0) -n*n/( (oax*oax + oay*oay)*(obx*obx + oby*oby))
    else n*n/( (oax*oax + oay*oay)*(obx*obx + oby*oby))
  }
}