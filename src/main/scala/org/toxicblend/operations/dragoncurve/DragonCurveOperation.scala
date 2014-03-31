package org.toxicblend.operations.dragoncurve

import org.toxicblend.CommandProcessorTrait
import scala.collection.mutable.ArrayBuffer
import toxi.geom.Vec3D
import toxi.geom.ReadonlyVec3D
import toxi.geom.LineStrip3D
import org.toxicblend.util.Time.time
import org.toxicblend.protobuf.ToxicBlendProtos.Message
import org.toxicblend.typeconverters.LineStripConverter
import org.toxicblend.typeconverters.OptionConverter
import org.toxicblend.util.Regex
import scala.collection.JavaConversions._

/**
 * This class is a Scala port of http://rosettacode.org/wiki/Dragon_curve#Java
 * Author unknown
 * License: same as rosettacode.org code
 */
class DragonCurveOperation extends CommandProcessorTrait {
  
  def processInput(inMessage:Message, options:OptionConverter) = {
    val traceMsg = "DragonCurveOperation"
    val iterations = options.getIntProperty("iterations", 9, traceMsg)   
    val edgeLength = options.getFloatProperty("edgeLength", 1f, traceMsg)
    val cursorPos = options.getCursorPosProperty(traceMsg)
    time("Dragon curve operation:", Message.newBuilder.addModels({
      val dragon = DragonCurveOperation.draw(DragonCurveOperation.generateData(iterations), edgeLength)
      val model = LineStripConverter(dragon,"dragon curve") 
      model.center(cursorPos).toPBModel(uniqueVertices=false) // don't run remove doubles
    }))
  }
}

object DragonCurveOperation {
  
  /**
   * start with original string="1"
   * add 1 to the right hand side of original string
   * add the original string with the middle digit reversed to get the new string
   * original string = new string
   * repeat
   */
  def generateData(folds:Int):IndexedSeq[Boolean] = {
    val directions = new ArrayBuffer[Boolean]
    directions.append(true)
    for(count <- 0 until folds ){
      val firstHalf = directions.slice(0,directions.length/2)
      val middleFlipped = !directions(directions.length/2)
      val lastHalf = directions.slice(directions.length/2+1,directions.length)
      directions.append(true)
      directions ++= firstHalf
      directions +=  middleFlipped
      directions ++= lastHalf
    }
    directions
  }
  
  def draw(directionData:Seq[Boolean], edgeLength:Float) = {
    val rv = new LineStrip3D
    var x1 = 0f
    var y1 = 0f
    var x2 = 0f
    var y2 = 1f
    var x3 = 0f
    var y3 = 1f
    
    // read through generator string and
    // move left or right according to each digit
    // and current direction

    directionData.foreach(direction => {
      if ((y2-y1 ==  1) && !direction) x3=x2-1
      if ((y2-y1 == -1) && !direction) x3=x2+1
      if ((x2-x1 ==  1) && !direction) y3=y2+1
      if ((x2-x1 == -1) && !direction) y3=y2-1
      if ((y2-y1 ==  1) &&  direction) x3=x2+1
      if ((y2-y1 == -1) &&  direction) x3=x2-1
      if ((x2-x1 ==  1) &&  direction) y3=y2-1
      if ((x2-x1 == -1) &&  direction) y3=y2+1
      rv.add(x3*edgeLength, y3*edgeLength, 0f)
      x1=x2
      y1=y2
      x2=x3
      y2=y3
    })
    rv
  }
}