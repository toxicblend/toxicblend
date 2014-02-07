package org.toxicblend.operations.simplegcodeparse

import scala.collection.mutable.ArrayBuffer
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D

/**
 * container for the parsed gcode operation
 */
class GcodeLines(val commands:List[GcodeLine]) {
  override def toString = commands.toString
  
  /**
   * Generates a sequence of state, on step for each active line in the gcode file
   * @param scaleMultiplier gcode (in metric mode) uses mm, while blender (in default metric mode) uses meter. This scaleMultiplier fixes that
   */
  def getSegments(scaleMultiplier:Float):IndexedSeq[Segment] = {
     val rv = new ArrayBuffer[Segment]
     val state = new InternalState(new Vec3D, new Vec3D, 0, 0)
     commands.foreach(aCommand => aCommand match {
       case command:GcodeMultiCommand => 
         if (command.key=="G0" || command.key=="G1") {
           command.steps.foreach(step => {
             step.foreach( argument => {
               argument.key match {
                 case "F" => if (command.key == "G0") state.f0 = argument.value else state.f1 = argument.value
                 case "X" => state.p1.x = argument.value*scaleMultiplier
                 case "Y" => state.p1.y = argument.value*scaleMultiplier
                 case "Z" => state.p1.z = argument.value*scaleMultiplier
                 case _ => println("Unkown argument" + argument.key)
               }
             }) 
             rv += new Segment(command.key, state.p0.copy, state.p1.copy, state.f0, state.f1)
             state.p0.x = state.p1.x
             state.p0.y = state.p1.y
             state.p0.z = state.p1.z
           })
         }
       case setting:GcodeSingleCommand => println("ignoring setting" + setting)
     })
     rv
  }
  protected class InternalState(val p0:Vec3D, val p1:Vec3D, var f0:Float, var f1:Float) 
  class Segment(val command:String, val p0:ReadonlyVec3D, val p1:ReadonlyVec3D, val f0:Float, val f1:Float){
    override def toString = command + " " + p0 + "->" + p1 + " f0=" + f0 + " f1=" + f1
  }
}

object GcodeLines {
  def apply (inputCommands:List[List[GcodeLine]]) = {
    new GcodeLines(inputCommands.flatten)
  }
}