package org.toxicblend.operations.simplegcodegenerator

import scala.collection.mutable.ArrayBuffer

/**
 * Everything (but blenderUnitToMM) in here is in millimeter, mm/s and rpm as applicable
 */
class GCodeSettings(val blenderUnitToMM:Float,
                    val outFilename:String,
                    val safeZ:Float,
                    val g0Feedrate:Float,
                    val g1Feedrate:Float,
                    val g1PlungeFeedrate:Float,
                    val spindleSpeed:Float,
                    val g64Command:String,
                    val customEndCommand:String,
                    val stepDown:Float ) {
  
  val safeZAsString = GCode.floatToString(safeZ)
  val g0FeedrateAsString = GCode.floatToString(g0Feedrate)
  val g1FeedrateAsString = GCode.floatToString(g1Feedrate)
  val g1PlungeFeedrateAsString = GCode.floatToString(g1PlungeFeedrate)
  val spindleSpeedAsString = GCode.floatToString(spindleSpeed)
  val stepDownAsString = GCode.floatToString(stepDown)
  
  val sizeX:Option[Float] = None
  val sizeY:Option[Float] = None
  val sizeZ:Option[Float] = None
  
  def toString(uncommentedHeader:String) = {
    "(" + uncommentedHeader + ")\n" +
    "(             safeZ=" + safeZAsString + ")\n" +
    "(        g0Feedrate=" + g0FeedrateAsString + ")\n" +
    "(        g1Feedrate=" + g1FeedrateAsString + ")\n" +
    "(  g1PlungeFeedrate=" + g1PlungeFeedrateAsString + ")\n" +
    "(      spindleSpeed=" + spindleSpeedAsString + ")\n" +
    "(          stepDown=" + stepDownAsString + ")\n"+
    "\n"
  }  
}
                    
