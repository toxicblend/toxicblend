package org.toxicblend.util

/**
 * A simple time measuring device
 */
object Time {
  
  def formatAsSeconds(spentTime:Float) = {
    if (spentTime > 1000f) {
      "%.3f s".format(spentTime/1000f)
    } else {
      "%.3f ms".format(spentTime)
    }
  }
  
  def time[A](text:String="time: ", f: => A) = {
    
    val (spentTime, returnValue) = {
      val timeSample = System.nanoTime
      val rv = f
      ((System.nanoTime-timeSample)/1e6f, rv)
    }
    println(text + formatAsSeconds(spentTime))
    returnValue
  }
}