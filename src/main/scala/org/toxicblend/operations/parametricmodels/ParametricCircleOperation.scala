package org.toxicblend.operations.parametricmodels

import org.toxicblend.CommandProcessorTrait
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.typeconverters.Mesh3DConverter
import org.toxicblend.util.Regex
import org.toxicblend.UnitSystem
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import toxi.geom.Matrix4x4
import org.toxicblend.util.Time

/**
 * A 'draw custom geometry' example 
 */
class ParametricCircleOperation extends CommandProcessorTrait {
  
  protected def addEdgeWithOrigin(e1:ReadonlyVec3D, e2:ReadonlyVec3D, origin:ReadonlyVec3D, mesh:Mesh3DConverter) = {
    mesh.addEdge(e1.add(origin), e2.add(origin))
  }
  
  /*
   * a totally unreadable circle drawer :) 
   */
  def drawCircle(options:OptionConverter, origin:ReadonlyVec3D):Mesh3DConverter = {
    val rotation = new Matrix4x4
    val rv = new Mesh3DConverter("parametric circle")
    val circleData = new Array[Vec3D](128)
    
    def circle(index:Int):Vec3D = {
      if (index < 0) {
        circleData(index+circleData.size)
      } else if (index >= circleData.size){
        circleData(index-circleData.size)
      } else {
        circleData(index)
      }
    }
    
    // each "cell" occupies this number of radians 
    val deltaDegree = (2d*Math.PI) / circleData.size
    val decoRotation = deltaDegree*.08
    var degree = 0d
    (0 until circleData.size).foreach(i => {
      circleData(i) = new Vec3D(Math.cos(degree).toFloat, Math.sin(degree).toFloat, 0)
      degree += deltaDegree
    })
    circleData.sliding(2).foreach(l => addEdgeWithOrigin(l(0),l(1),origin,rv))
    addEdgeWithOrigin(circleData.last,circle(0),origin,rv)
    
    var radius = 1f
    var steps = 1
    val deltaRadius = -0.1f
    var indexOffset = 0
    val stepsI = Array(2,2,4,4,4,8,8,16,16,32).iterator
    (0 until 10).foreach(l => {
      
      val circum = 2.*Math.PI*radius
      //println("circum=" + circum + " radius = " + radius + " steps:" + steps + " circum/steps=" + circum/steps)

      // draw connecting edges
      steps = stepsI.next //(.5+ 1.5f/circum).toInt 
      (0 until (circleData.size, steps) ).foreach( s => {
         addEdgeWithOrigin(circle(s+indexOffset-1).scale(radius+deltaRadius),circle(s+indexOffset).scale(radius),origin,rv)
      })
      // draw circles
      if (l > 0){
        circleData.sliding(2).foreach(l => addEdgeWithOrigin(l(0).scale(radius),l(1).scale(radius),origin,rv))
        addEdgeWithOrigin(circleData.last.scale(radius),circle(0).scale(radius),origin,rv)
      }
      radius += deltaRadius
      //rotation.rotateZ(decoRotation)
      //circleData.foreach(v => rotation.applyToSelf(v))
      indexOffset-=1
    })
    rv
  }
  
  def processInput(inMessage:Message, options:OptionConverter) = {
    
    val traceMsg = "ParametricCircleOperation"
    val useMultiThreading = options.getMultiThreadingProperty(traceMsg)
    if (useMultiThreading) if (useMultiThreading) System.err.println(traceMsg + ":useMultiThreading=True but it's not implemented yet")
    val drawType = options.getOrElse("drawTypeProperty", "CIRCLE").toUpperCase() match {
      case "CIRCLE" => "CIRCLE"
      case "BOX" => "BOX"
      case s:String => System.err.println("ParametricCircleOperation: Unrecognizable 'drawTypeProperty' property value: " +  s ); "CIRCLE"
    }
    val unitScale = options.getUnitScaleProperty(traceMsg)
    val unitSystem = options.getUnitSystemProperty(traceMsg)
    
    val returnMessageBuilder = Message.newBuilder()
    val returnMeshConverter = Time.time("Building parametric circle:",
      if (drawType == "CIRCLE"){
        drawCircle(options, options.getCursorPosProperty(traceMsg))
      } else {
        new Mesh3DConverter
      }
    )
    returnMessageBuilder.addModels(returnMeshConverter.toPBModel(None, None))
    returnMessageBuilder
  }
}