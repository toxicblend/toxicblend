package org.toxicblend

class ToxicblendException (val text:String) extends Exception{
  override def toString :String = {
    "ToxicblendException: " + text
  }
}