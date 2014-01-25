//
//  medianAxisjni.cpp
//
//  Created by EAD Fritz on 2013-05-20.
//  Copyright (c) 2013 EAD Fritz. All rights reserved.
//

#include "org_toxicblend_operations_boostmedianaxis_MedianAxisJni.hpp"
#include <boost/thread/mutex.hpp>
#include "medianaxis.hpp"
#include <boost/geometry.hpp>
#include <boost/geometry/geometry.hpp>
#include <boost/geometry/geometries/linestring.hpp>

typedef std::map<long, medianAxisFloat::Medianaxis*> map_t;
typedef map_t::iterator map_i;

static map_t medianaxis_MedianAxisJni_instanceMap;
static boost::mutex medianaxis_MedianAxisJni_mtx;
static long medianaxis_MedianAxisJni_instanceCounter = 0;

static char exceptionBuffer[200];

inline medianAxisFloat::Medianaxis* medianaxis_MedianAxisJni_mapInstance(jlong instanceId){
  medianAxisFloat::Medianaxis* rv = NULL;
  medianaxis_MedianAxisJni_mtx.lock();
  map_i i = medianaxis_MedianAxisJni_instanceMap.find(instanceId);
  if(i != medianaxis_MedianAxisJni_instanceMap.end()) {
    rv = i->second;
  }
  medianaxis_MedianAxisJni_mtx.unlock();
  return rv;
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    allocateJni_
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_allocateJni_1(JNIEnv *env, jobject javaThis){
  medianaxis_MedianAxisJni_mtx.lock();
  jlong instanceId = ++medianaxis_MedianAxisJni_instanceCounter;
  medianaxis_MedianAxisJni_instanceMap[instanceId] = new medianAxisFloat::Medianaxis();
  medianaxis_MedianAxisJni_mtx.unlock();
  
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_ new object " << instanceId <<  std::endl;
  return instanceId;
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    deallocateJni_
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_deallocateJni_1(JNIEnv * env, jobject javaThis, jlong instanceId){
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_deallocateJni_1 delete object " << instanceId <<  std::endl;
  medianaxis_MedianAxisJni_mtx.lock();
  
  map_i i = medianaxis_MedianAxisJni_instanceMap.find(instanceId);
  if(i != medianaxis_MedianAxisJni_instanceMap.end()){
    delete i->second;
  	medianaxis_MedianAxisJni_instanceMap.erase(i);
  } else {
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_deallocateJni_1 " << instanceId <<  " is invalid " << std::endl;
  }
  medianaxis_MedianAxisJni_mtx.unlock();
  return;
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    voronoiInternalEdgesJni_
 * Signature: (J[I[IFF)[F
 */
JNIEXPORT jfloatArray JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_voronoiInternalEdgesJni_1(
  JNIEnv *env, jobject javaThis, jlong instanceId, jintArray inJNIArray_outer, jintArray inJNIArray_holes,
  jfloat zEpsilon, jfloat dotProductLimit, jfloat resolution){
  
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_voronoiInternalEdgesJni_1 " << instanceId <<  std::endl;
  medianAxisFloat::Medianaxis* ma = medianaxis_MedianAxisJni_mapInstance(instanceId);
  if (NULL==ma){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_voronoiInternalEdgesJni_1 " << instanceId <<  " is invalid " << std::endl;
    return NULL;
  }
  
  // Step 1: Convert the incoming JNI jintArray to C's jint[]
  jboolean isCopy1 = false;
  jint *inCArrayOuter = env->GetIntArrayElements(inJNIArray_outer, &isCopy1);
  if (NULL == inCArrayOuter){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_voronoiInternalEdgesJni_1 " << instanceId <<   " failed to read input." << std::endl;
    return NULL;
  }
  jsize inCArrayOuterLength = env->GetArrayLength(inJNIArray_outer);
    
  jboolean isCopy2 = false;
  jint *inCArrayHole = env->GetIntArrayElements(inJNIArray_holes, &isCopy2);
  if (NULL == inCArrayHole){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_voronoiInternalEdgesJni_1 " << instanceId <<   " failed to read input." << std::endl;
    return NULL;
  }
  jsize inCArrayHoleLength = env->GetArrayLength(inJNIArray_holes);
  
  // Step 2: compute
  float* outCArray = NULL;
  jsize outCArrayLength=0;
  
  bool trace = false;
  DBG trace = true;
  ma->voronoiInternalEdges(inCArrayOuter, inCArrayOuterLength, inCArrayHole, inCArrayHoleLength, outCArray, outCArrayLength, zEpsilon, dotProductLimit, resolution);
  env->ReleaseIntArrayElements(inJNIArray_outer, inCArrayOuter, 0); // release resources
  env->ReleaseIntArrayElements(inJNIArray_holes, inCArrayHole, 0); // release resources

  // Step 3: Convert the C's Native jdouble[] to JNI jfloatArray, and return
  jfloatArray outJNIArray = env->NewFloatArray(outCArrayLength);  // allocate
  if (NULL == outJNIArray) return NULL;
  env->SetFloatArrayRegion(outJNIArray, 0 , outCArrayLength, outCArray);  // copy
  if (NULL!=outCArray) {
    delete [] outCArray;
    outCArray = NULL;
  }

  return outJNIArray;
}


/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    getRingJni_
 * Signature: (JI)[F
 */
JNIEXPORT jfloatArray JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_getRingJni_1(JNIEnv *env, jobject javaThis, jlong instanceId, jint ringId) {
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_getRingJni_1 " << instanceId <<  " ringId: " << ringId << std::endl;
  medianAxisFloat::Medianaxis* ma = medianaxis_MedianAxisJni_mapInstance(instanceId);
  if (NULL==ma){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_getRingJni_1 " << instanceId <<  " is invalid " << std::endl;
    return NULL;
  }
  
  medianAxisFloat::ArrayContainerFloat_t* storedRing= ma->getRing(ringId);
  if (NULL==storedRing){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_getRingJni_1 " << instanceId <<  " could not find ringId: " << ringId << std::endl;
    return NULL;
  }
  
  // Step 3: Convert the C's Native jdouble[] to JNI jfloatArray, and return
  jfloatArray outJNIArray = env->NewFloatArray(storedRing->length);  // allocate
  if (NULL == outJNIArray) return NULL;
  env->SetFloatArrayRegion(outJNIArray, 0 , storedRing->length, storedRing->data);  // copy
  
  return outJNIArray;
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    simplify2D_
 * Signature: ([FF)[F
 */
JNIEXPORT jfloatArray JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_simplify2D_1(
  JNIEnv *env, jobject javaThis, jfloatArray inJNIArray, jfloat limit){
  
  typedef boost::geometry::model::d2::point_xy<double> point_2d;
  typedef boost::geometry::model::linestring<point_2d> linestring_2d;
  jboolean isCopy = false;
  jfloat *inCArray = env->GetFloatArrayElements(inJNIArray, &isCopy);
  jsize inCarraySize = env->GetArrayLength(inJNIArray);

  linestring_2d line;
  
  for (int i=0; i < inCarraySize-1; i+=2) {
    line.push_back(boost::geometry::make<point_2d>(inCArray[i], inCArray[i+1]));
  }
  
  // Simplify it, using distance
  linestring_2d simplified;
  boost::geometry::simplify(line, simplified, limit);
  DBG std::cout
  	<< "         original: " << boost::geometry::dsv(line) << std::endl
  	<< "  original.size(): " << line.size()  << std::endl
  	<< "       simplified: " << boost::geometry::dsv(simplified) << std::endl
  	<< "simplified.size(): " << simplified.size()  << std::endl ;
  
  jsize outCarraySize = (jsize) simplified.size()*2;
  jfloat* outCarray = new jfloat[outCarraySize];
  for (int i=0; i<simplified.size(); i++){
    outCarray[i*2+0] = (jfloat) simplified[i].x();
    outCarray[i*2+1] = (jfloat) simplified[i].y();
  }
  
  // Step 3: Convert the C's Native jdouble[] to JNI jfloatArray, and return
  jfloatArray outJNIArray = env->NewFloatArray(outCarraySize);  // allocate
  if (NULL == outJNIArray) return NULL;
  env->SetFloatArrayRegion(outJNIArray, 0 , outCarraySize, outCarray);  // copy
  env->ReleaseFloatArrayElements(inJNIArray, inCArray, 0); // release resources
  return outJNIArray;
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    simplify3D_
 * Signature: ([FF)[F
 */
JNIEXPORT jfloatArray JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_simplify3D_1(                                                                                                  
  JNIEnv *env, jobject javaThis, jfloatArray inJNIArray, jfloat limit){
  
  typedef boost::geometry::model::point<double, 3, boost::geometry::cs::cartesian> point_3d;
  typedef boost::geometry::model::linestring<point_3d> linestring_3d;
  
  linestring_3d line;
  jboolean isCopy = false;
  jfloat *inCArray = env->GetFloatArrayElements(inJNIArray, &isCopy);
  jsize inCarraySize = env->GetArrayLength(inJNIArray);
  
  for (int i=0; i < inCarraySize-2; i+=3) {
    //boost::geometry::append(line,input +i);
    //line.push_back(oost::geometry::make<point_3d>(input +i));
    line.push_back(boost::geometry::make<point_3d>(inCArray[i], inCArray[i+1], inCArray[i+2]));
  }
  
  // Simplify it, using distance
  linestring_3d simplified;
  boost::geometry::simplify(line, simplified, limit);
  DBG std::cout
    << "simplify 3d limit: " << limit << std::endl
  	<< "         original: " << boost::geometry::dsv(line) << std::endl
  	<< " original.size() : " << line.size()  << std::endl
  	<< "       simplified: " << boost::geometry::dsv(simplified) << std::endl
  	<< "simplified.size(): " << simplified.size()  << std::endl ;
  
  jsize outCarraySize = (jsize) simplified.size()*3;
  jfloat* outCarray = new jfloat[outCarraySize];
  for (int i=0; i<simplified.size(); i++){
    register int j=i*3;
    outCarray[j+0] = (jfloat) simplified[i].get<0>();
    outCarray[j+1] = (jfloat) simplified[i].get<1>();
    outCarray[j+2] = (jfloat) simplified[i].get<2>();
  }
  // Step 3: Convert the C's Native jdouble[] to JNI jfloatArray, and return
  jfloatArray outJNIArray = env->NewFloatArray(outCarraySize);  // allocate
  if (NULL == outJNIArray) return NULL;
  env->SetFloatArrayRegion(outJNIArray, 0 , outCarraySize, outCarray);  // copy
  env->ReleaseFloatArrayElements(inJNIArray, inCArray, 0); // release resources
  return outJNIArray;
}


/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    setRingJni_
 * Signature: (JI[FF)V
 */
JNIEXPORT void JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_setRingJni_1(
  JNIEnv *env, jobject javaThis, jlong instanceId, jint ringId, jfloatArray inJNIArray, jfloat limit){
  
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_setRingJni " << instanceId <<  " ringId: " << ringId << std::endl;
  medianAxisFloat::Medianaxis* ma = medianaxis_MedianAxisJni_mapInstance(instanceId);
  if (NULL==ma){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_setRingJni " << instanceId <<  " is invalid " << std::endl;
    return;
  }
  
  // Step 1: Convert the incoming JNI jfloatArray to C's jfloat[]
  jboolean isCopy = false;
  jfloat *inCArray = env->GetFloatArrayElements(inJNIArray, &isCopy);
  if (NULL == inCArray){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_setRing " << instanceId <<   " failed to read input." << std::endl;
    return;
  }
  jsize length = env->GetArrayLength(inJNIArray);
  ma->setRing(ringId, inCArray, length, limit); // copy array
  
  if (ma->ringIsIntersectingItself(ringId)){
    sprintf(exceptionBuffer, " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_setRingJni: newly added ring %d is intersecting itself", ringId);
    env->ThrowNew(env->FindClass("java/lang/Exception"), exceptionBuffer);
  }
  env->ReleaseFloatArrayElements(inJNIArray, inCArray, 0); // release resources
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    addRingJni_
 * Signature: (J[FF)I
 */
JNIEXPORT jint JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_addRingJni_1(JNIEnv *env, jobject javaThis, jlong instanceId, jfloatArray inJNIArray, jfloat simplifylimit){
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_addRingJni " << instanceId <<  std::endl;
  medianAxisFloat::Medianaxis* ma = medianaxis_MedianAxisJni_mapInstance(instanceId);
  if (NULL==ma){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_addRingJni " << instanceId <<  " is invalid " << std::endl;
    return 0;
  }
  
  // Step 1: Convert the incoming JNI jfloatArray to C's jdouble[]
  jboolean isCopy = false;
  jfloat *inCArray = env->GetFloatArrayElements(inJNIArray, &isCopy);
  if (NULL == inCArray){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_addRingJni " << instanceId <<  " failed to read input." << std::endl;
    return -1;
  }
  jsize length = env->GetArrayLength(inJNIArray);
  jint ringId = ma->addRing(inCArray, length, simplifylimit);  // copy array
  
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_addRingJni " << instanceId <<  " added ringId: " << ringId << std::endl;

  env->ReleaseFloatArrayElements(inJNIArray, inCArray, 0); // release resources
  
  if (ma->ringIsIntersectingItself(ringId)){
    sprintf(exceptionBuffer, " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_addRingJni: newly added ring %d is intersecting itself", ringId);
    env->ThrowNew(env->FindClass("java/lang/Exception"), exceptionBuffer);
  }
  return ringId;
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    ringContainsAllPointsJni_
 * Signature: (J[FI)Z
 */
JNIEXPORT jboolean JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_ringContainsAllPointsJni_1
  (JNIEnv *env, jobject javaThis, jlong instanceId, jfloatArray inJNIArray, jint ringId){
  
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_pointsInRingJni " << instanceId <<  " ringId: " << ringId << std::endl;
  medianAxisFloat::Medianaxis* ma = medianaxis_MedianAxisJni_mapInstance(instanceId);
  if (NULL==ma){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_pointsInRingJni " << instanceId <<  " is invalid " << std::endl;
    return false;
  }
  
  // Step 1: Convert the incoming JNI jfloatArray to C's jdouble[]
  jboolean isCopy = false;
  jfloat *inCArray = env->GetFloatArrayElements(inJNIArray, &isCopy);
  if (NULL == inCArray){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_pointsInRing " << instanceId <<  " ringId: " << ringId << " failed to read input."<< std::endl;
    return false;
  }
  jsize length = env->GetArrayLength(inJNIArray);
  bool rv = ma->ringContainsAllPoints(inCArray, length, ringId);
  
  env->ReleaseFloatArrayElements(inJNIArray, inCArray, 0); // release resources
  return rv;
}

/*
 * Class:     org_toxicblend_operations_medianaxis_MedianAxisJni
 * Method:    ringContainsRingJni_
 * Signature: (JII)Z
 */
JNIEXPORT jboolean JNICALL Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_ringContainsRingJni_1
(JNIEnv *env, jobject javaThis, jlong instanceId, jint outerRingId, jint innerRingId) {
  DBG std::cout << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_ringContainsRingJni_1 " << instanceId <<  " outerRingId: " << outerRingId <<  " innerRingId: " << innerRingId << std::endl;
  medianAxisFloat::Medianaxis* ma = medianaxis_MedianAxisJni_mapInstance(instanceId);
  if (NULL==ma){
    std::cerr << " Java_org_toxicblend_operations_boostmedianaxis_MedianAxisJni_ringContainsRingJni_1 " << instanceId <<  " is invalid " << std::endl;
    return false;
  }
  
  bool rv = ma->ringContainsRing(outerRingId, innerRingId);
  
  return rv;
}
