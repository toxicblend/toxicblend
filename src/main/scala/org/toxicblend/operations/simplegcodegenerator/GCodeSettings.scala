package org.toxicblend.operations.simplegcodegenerator

/**
 * Everything in here is in millimeter, mm/s and rpm as applicable
 */
class GCodeSettings(val outFilename:String,
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
  
  // TODO: give sizeX,sizeY,sizeZ proper values/settings
  val sizeX:Option[Float] = None
  val sizeY:Option[Float] = None
  val sizeZ:Option[Float] = None

}
                    
