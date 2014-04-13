package org.toxicblend.util

/**
 * A simple time measuring device
 */
object Time {
  def time[A](text:String="time: ", f: => A) = {
    
    val (spentTime, returnValue) = {
      val timeSample = System.nanoTime
      val rv = f
      ((System.nanoTime-timeSample)/1e6f, rv)
    }
    val timeStr = if (spentTime > 1000f) {
      "%fs".format(spentTime/1000f)
    } else {
      "%fms".format(spentTime)
    }
    println(text + timeStr)
    returnValue
  }
}