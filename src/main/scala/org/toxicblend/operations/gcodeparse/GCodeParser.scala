package org.toxicblend.operations.gcodeparse

import scala.util.parsing.combinator.RegexParsers
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import scala.collection.mutable.ArrayBuffer
import scala.annotation.migration

class GcodeStepArgument(val key:String, val value:Float) {
  override def toString = key+value
}

class GcodeStep(val stepData:List[GcodeStepArgument]) {
  override def toString = stepData.toString
}

class GcodeCommand(val key:String, val steps:List[GcodeStep]) {
  override def toString = "key:" + key + " " + steps
}

class GcodeCommands(val commands:List[GcodeCommand]) {
  override def toString = commands.toString
  /**
   * Generates a sequence of state, on step for each active line in the gcode file
   */
  def getSegments:IndexedSeq[Segment] = {
     val rv = new ArrayBuffer[Segment]
     val state = new State(new Vec3D,new Vec3D, 0, 0)
     commands.foreach(command => {
         if (command.key=="G0" || command.key=="G1") {
           command.steps.foreach(step => {
             step.stepData.foreach( argument => {
               argument.key match {
                 case "F" => if (command.key == "G0") state.f0 = argument.value else state.f1 = argument.value
                 case "X" => state.p1.x = argument.value
                 case "Y" => state.p1.y = argument.value
                 case "Z" => state.p1.z = argument.value
                 case _ => println("Unkown argument" + argument.key)
               }
             }) 
             rv += new Segment(command.key, state.p0.copy, state.p1.copy, state.f0, state.f1)
             state.p0.x = state.p1.x
             state.p0.y = state.p1.y
             state.p0.z = state.p1.z
           })
         }
     })
     rv
  }
  class State(val p0:Vec3D, val p1:Vec3D, var f0:Float, var f1:Float) 
  class Segment(val command:String, val p0:ReadonlyVec3D, val p1:ReadonlyVec3D, val f0:Float, val f1:Float){
    override def toString = command + " " + p0 + "->" + p1 + " f0=" + f0 + " f1=" + f1
  }
}

/** 
 *  Parses the gcode text into an object hierarchy
 */
class GCodeParser extends RegexParsers {
  val NEWLINE = String.format("%n") // """\n""" is sometimes """\r\n""" :/
  override val skipWhitespace = false
      
  def gcodeCommands: Parser[GcodeCommands] = lineNumber.? ~> rep1sep(gcodeCommand,NEWLINE) ^^ {  
    case commands => new GcodeCommands(commands)
  }
  
  def gcodeCommand: Parser[GcodeCommand] = lineNumber.? ~> (gCommandName ~ gcodeSteps) ^^ {
    case gCommandName~gArgumentsLines => new GcodeCommand(gCommandName.capitalize,gArgumentsLines)
  }
  
  def gCommandName =  """(?i)(?:G0?0)|(?:G0?1)""".r
  // any G except G0 and G1.  G0 and G1 should always be separated by a newline (imho)
  def gSettingName =  """(?i)(?:G[123456789]?\d)(?:G0?[23456789])|(?:M\d?\d?\d)""".r 

  /**
   * A list command steps. e.g. matching:"X43.2 F1000 Z-1\nX2 Z1"
   */
  def gcodeSteps: Parser[List[GcodeStep]] = rep1sep(gcodeLine,NEWLINE) 
  
  /**
   * A list of command arguments representing one step. e.g. matching:"X43.2 F1000 Z-1"
   */
  def gcodeLine: Parser[GcodeStep] = lineNumber.? ~> rep1(gcodeStepArgument) ^^ { 
    case arguments => new GcodeStep(arguments)
  } 
  
  /**
   * a single argument to the commands. e.g matching: "X43.2", "F1000", "Z-1"
   */
  def gcodeStepArgument:Parser[GcodeStepArgument] = (gArgumentKey ~ fNumber) ^^ {
    case gArgumentKey~fNumber => new GcodeStepArgument(gArgumentKey.capitalize,fNumber) 
  }
  def gArgumentKey = """(?i)[XYZIJFPQS]""".r
  def fNumber: Parser[Float] = """-?\d+(?:\.(?:\d*)?)?(?:[E|e]-?\d+)?""".r ^^ { _.toFloat }
  def lineNumber = """(?i)N\d+""".r
  //def emptyline = "^/n".r
  
  /**
   * remove non-nested comments and whitespace
   * This is done in a super inefficient two pass. TODO: figure out how to avoid these steps
   */
  def filterOutWhiteSpace(input:String) = {
    val tmp = """(?s)(?:[ \t\r\f]+)|(?:\([^\)]*\))""".r.replaceAllIn(input, "") // remove non-nested comments and whitespace
    """(?m)(?:^\n)+""".r.replaceAllIn(tmp, "") // remove empty lines
  }
}

object GCodeParser {
  def main(args: Array[String]) {
   val objParser = new GCodeParser
   var arg1 = "N0G0 Z2.0 (goto safe z)\nn0g0  x1 y2  z3 (comme\nnt comment)\n  N  1 g1 x2 y2 z3"
   var arg = """
G0 Z2.0 (goto safe z)
M3 S1000 (start splindle)
G4 P3  (dwell 3 seconds)
G0 F1000 (set rapid feedrate)
G1 F500 (set normal feedrate)
G64 P0.02 Q0.02
G0 Z2.000000
G0 X19.01426 Y26.41212
G1 X19.01426 Y26.41212 Z0 F500.000000
G1 X19.01426 Y26.41212 Z-11.68791 F100.000000
G1 X19.01426 Y26.41212 Z-11.68791 F500.000000
G1 X19.01426 Y26.41212 Z-11.68791
 X19.01426 Y26.41212 Z-11.68791
G1 X20.81396 Y27.20154 Z-13.37177 
G1 X23.99418 Y24.75371 Z-11.88044 
G1 X26.76230 Y23.35208 Z-10.87037 
G1 X30.86569 Y22.06351 Z-9.78996 
G1 X35.54356 Y21.31058 Z-9.02444 
G1 X40.85203 Y21.03279 Z-8.58315 
G1 X48.17540 Y21.27965 Z-8.31863 
G1 X61.07601 Y22.72789 Z-7.92359 
"""
   arg = objParser.filterOutWhiteSpace(arg)
   val rv = objParser.parse(objParser.gcodeCommands, arg)
   println("rest:" + rv.next.source.toString.substring(rv.next.offset))
   val objects = rv.get
   println("input : " + arg + "\noutput:" + objects)
   //println("output : " + objects + " len:" + objects.getSegments.size)
   //println("output : \n" + objects.getSegments.mkString("\n"))
  }
}