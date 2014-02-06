package org.toxicblend.operations.gcodeparse

import scala.util.parsing.combinator.RegexParsers
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import scala.collection.mutable.ArrayBuffer
import scala.annotation.migration

class GcodeParameter(val key:String, val value:Float) {
  override def toString = key+value
}

/**
 * base class for single and multi line commands
 */
class GcodeLine
/**
 * Gcode command where the parameters only can occur once
 */
class GcodeSingleCommand(val key:String,val parameters:Option[List[GcodeParameter]]) extends GcodeLine {
  override def toString = key
}
/**
 * Gcode command where the parameters only can occur more than once (separate lines though)
 */
class GcodeMultiCommand(val key:String, val steps:List[List[GcodeParameter]]) extends GcodeLine {
  override def toString = "key:" + key + " " + steps
}

class GcodeLines(inputCommands:List[List[GcodeLine]]) {
  val commands = inputCommands.flatten
  override def toString = commands.toString
  /**
   * Generates a sequence of state, on step for each active line in the gcode file
   */
  def getSegments:IndexedSeq[Segment] = {
     val rv = new ArrayBuffer[Segment]
     val state = new State(new Vec3D, new Vec3D, 0, 0)
     commands.foreach(aCommand => aCommand match {
       case command:GcodeMultiCommand => 
         if (command.key=="G0" || command.key=="G1") {
           command.steps.foreach(step => {
             step.foreach( argument => {
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
       case setting:GcodeSingleCommand => println("ignoring setting" + setting)
     })
     rv
  }
  class State(val p0:Vec3D, val p1:Vec3D, var f0:Float, var f1:Float) 
  class Segment(val command:String, val p0:ReadonlyVec3D, val p1:ReadonlyVec3D, val f0:Float, val f1:Float){
    override def toString = command + " " + p0 + "->" + p1 + " f0=" + f0 + " f1=" + f1
  }
}

/** 
 * Parses the gcode text into an object hierarchy.
 * G0 and G1 commands can look like this:
 * G0 X1 Y2 Z1
 *    X2 
 *    X3
 *    ..
 *    
 * 'Settings' can sometimes occur on the same line:
 * G40 G49 G54 G80 G90 G94
 * 
 * I make the assumtion that multiple G0 and G1 commands never occupy the same line  
 */
class GCodeParser extends RegexParsers {
  val NEWLINE = String.format("%n") // """\n""" is sometimes """\r\n""" :/
  override val skipWhitespace = false
  
  def gCode: Parser[GcodeLines] = rep((rep1sep(gMultiLineCommand,NEWLINE) | rep1(gSingleLineCommand))<~NEWLINE.? ) ^^ {
    case lines => new GcodeLines(lines)
  }
  
  /**
   * multiline commands (G0 and G1)
   */ 
  def gMultiLineCommand : Parser[GcodeLine] = """(?i)(?:G0?0)|(?:G0?1)""".r ~ gMultiLineParameters ^^ { 
    case name~parameters => new GcodeMultiCommand(name.capitalize,parameters)
  }
  
  /** 
   * single line commands . Any G except G0 and G1. G0 and G1 should always be separated by a newline (imho)
   */
  def gSingleLineCommand : Parser[GcodeLine] = ( """(?i)(?:G[123456789]\d)|(?:G0?[23456789])|(?:M\d?\d?\d)""".r ~ gSingleLineParameters.?) ^^ { 
    case name~parameters => new GcodeSingleCommand(name,parameters)
  }

  /**
   * A list (potentially multi line) command steps. e.g. matching:"X43.2 F1000 Z-1  \n  X2 Z1"
   */
  def gMultiLineParameters: Parser[List[List[GcodeParameter]]] = rep1sep(gSingleLineParameters,NEWLINE) 
  
  /**
   * A list of single line command arguments representing one step. e.g. matching:"X43.2 F1000 Z-1"
   */
  def gSingleLineParameters: Parser[List[GcodeParameter]] = rep1(gParameter) 
  
  /**
   * a single argument to the commands. e.g matching: "X43.2", "F1000", "Z-1"
   */
  def gParameter:Parser[GcodeParameter] = ("""(?i)[XYZIJFPQS]""".r ~ gParameterValue) ^^ {
    case gArgumentKey~fNumber => new GcodeParameter(gArgumentKey.capitalize,fNumber) 
  }
  
  def gParameterValue: Parser[Float] = """-?\d+(?:\.(?:\d*)?)?(?:[E|e]-?\d+)?""".r ^^ { _.toFloat }
  
  /**
   * remove non-nested comments and other whitespace
   * This is done in a super inefficient two pass. TODO: figure out how to avoid these steps
   */
  def filterOutWhiteSpace(input:String) = {
    val tmp = """(?s)(?:[ \t\r\f]+)|(?:\([^\)]*\))""".r.replaceAllIn(input, "") // remove non-nested comments and whitespace
    """(?i)(?m)(?:^\n)|(?:^N\d+)+""".r.replaceAllIn(tmp, "") // remove empty lines and line numbers
  }
}