package org.toxicblend.operations.simplegcodeparse

import scala.util.parsing.combinator.RegexParsers
import toxi.geom.ReadonlyVec3D
import toxi.geom.Vec3D
import scala.collection.mutable.ArrayBuffer
import scala.annotation.migration
import java.io.BufferedReader

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
 * I make the assumption that multiple G0 and G1 commands never occupy the same line  
 */
class GCodeParser extends RegexParsers {
  val NEWLINE = String.format("%n") // """\n""" is sometimes """\r\n""" :/
  override val skipWhitespace = false
  
  def gCode: Parser[GcodeLines] = rep((rep1sep(gMultiLineCommand,NEWLINE) | rep1(gSingleLineCommand))<~NEWLINE.? ) ^^ {
    case lines => GcodeLines(lines)
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
  
  /**
   * A float 
   * Accepted forms: 2., -1.0,  12, 1.2E3, -1.2E-3,5e-1    etc etc
   */
  def gParameterValue: Parser[Float] = """-?\d+(?:\.(?:\d*)?)?(?:[E|e]-?\d+)?""".r ^^ { _.toFloat }
  
  
  protected val commentsAndWhitespace = """(?s)(?:[ \t\r\f]+)|(?:\([^\)]*\))""".r
  protected val emptyLinesAndLineNumbers = """(?i)(?m)(?:^\n)|(?:^N\d+)+""".r
  
  /**
   * remove non-nested comments and other whitespace when using an Iterator[String] as input
   * This is done in a super inefficient two pass. TODO: figure out how to avoid these steps
   */
  def filterOutWhiteSpace(source: Iterator[String]) : String= {
    val sb = new StringBuilder 
    source.foreach(line => {
      val trimmed = commentsAndWhitespace.replaceAllIn(line, "")
      sb.append( emptyLinesAndLineNumbers.replaceAllIn(trimmed, ""))
    })
    // meh, do it all over again to fix multiline comments
    filterOutWhiteSpace(sb.result)
  }
  
  /**
   * remove non-nested comments and other whitespace
   * This is done in a super inefficient two pass. TODO: figure out how to avoid these steps
   */
  def filterOutWhiteSpace(input:java.lang.CharSequence) = {
    val tmp = commentsAndWhitespace.replaceAllIn(input, "") // remove non-nested comments and whitespace
    emptyLinesAndLineNumbers.replaceAllIn(tmp, "") // remove empty lines and line numbers
  }
}