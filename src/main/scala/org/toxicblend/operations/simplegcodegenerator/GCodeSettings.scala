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
  val safeZAsString = safeZ.toString().replace(",",".")
  val g0FeedrateAsString = g0Feedrate.toString().replace(",",".")
  val g1FeedrateAsString = g1Feedrate.toString().replace(",",".")
  val g1PlungeFeedrateAsString = g1PlungeFeedrate.toString().replace(",",".")
  val spindleSpeedAsString = spindleSpeed.toString().replace(",",".")
  val stepDownAsString = stepDown.toString().replace(",",".")
  
  // TODO: give sizeX,sizeY,sizeZ proper values/settings
  val sizeX:Option[Float] = None
  val sizeY:Option[Float] = None
  val sizeZ:Option[Float] = None

}
                    
