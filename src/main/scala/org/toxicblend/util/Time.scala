package org.toxicblend.util

object Time {
  def time[A](text:String="time: ", f: => A) = {
    val s = System.nanoTime
    val ret = f
    println(text+(System.nanoTime-s)/1e6+"ms")
    ret
  }
}