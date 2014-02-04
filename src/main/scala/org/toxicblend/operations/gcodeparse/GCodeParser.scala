package org.toxicblend.operations.gcodeparse

import java.io.FileReader
import scala.util.parsing.combinator.RegexParsers
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D

class GcodeStepArgument(val key:String, val value:Float) {
  override def toString = key+value
}

class GcodeStep(val step:List[GcodeStepArgument]) {
  override def toString = step.toString
}

class GcodeSteps(val steps:List[GcodeStep]) {
  override def toString = steps.toString
}

class GcodeCommand(val key:String, val lines:GcodeSteps) {
  override def toString = key + " " + lines
}

class GcodeCommands(val commands:List[GcodeCommand]) {
  override def toString = commands.toString
}

class GCodeParser extends RegexParsers {
  override val skipWhitespace = false
  
  
  def gcodeCommands: Parser[GcodeCommands] = repsep(gcodeCommand,CRLF) ^^ {
    case commands => new GcodeCommands(commands)
  }
  
  def gcodeCommand: Parser[GcodeCommand] =  (gCommandName ~ gArgumentsLines) ^^ {
    case gCommandName~gArgumentsLines => new GcodeCommand(gCommandName,gArgumentsLines)
  }
  
  def gCommandName = """(?i)G0|G1|G64""".r <~ SPACES.+
  
  /**
   * A list command steps. e.g. matching:"X43.2 F1000 Z-1\nX2 Z1"
   */
  def gArgumentsLines: Parser[GcodeSteps] = repsep(gcodeStep,CRLF) ^^ {
    case arguments => new GcodeSteps(arguments)
  }
  
  /**
   * A list of command arguments representing one step. e.g. matching:"X43.2 F1000 Z-1"
   */
  def gcodeStep: Parser[GcodeStep] = repsep(gcodeStepArgument,SPACES) <~ SPACES.? ^^ {
    case arguments => new GcodeStep(arguments)
  }
  
  /**
   * a single argument to the commands. e.g matching: "X43.2", "F1000", "Z-1"
   */
  def gcodeStepArgument:Parser[GcodeStepArgument] = (gArgumentKey ~ fNumber) ^^ {
    case gArgumentKey~fNumber => new GcodeStepArgument(gArgumentKey,fNumber) 
  }
  def CRLF    = """\r?\n""".r
  def SPACES  = """[ \t]+""".r // white space excluding line feed
  def WHITESPACE = """\s*""".r // all kinds of whitespace including linefeed
  def gArgumentKey= """(?i)[XYZIJFPQ]""".r
   
  def fNumber: Parser[Float] = """-?\d+(?:\.(?:\d*)?)?""".r ^^ { _.toFloat } // [ \t]*
}

object GCodeParser {
  def main(args: Array[String]) {
   val objParser = new GCodeParser
   val arg1 = "G0 x242 i-9.2345345  y34534 f245\nx12 z2"
   val objects = objParser.parse(objParser.gcodeCommands,arg1).get
   println("input : " + arg1)
   println("output : " + objects)
  }
}