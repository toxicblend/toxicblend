package org.toxicblend.tests

import collection.mutable.Stack
import org.scalatest._
import org.toxicblend.operations.gcodeparse.GCodeParser
import org.toxicblend.operations.gcodeparse.GcodeMultiCommand
import org.toxicblend.operations.gcodeparse.GcodeSingleCommand
import org.toxicblend.operations.gcodeparse.GcodeLines
import org.toxicblend.operations.gcodeparse.GcodeParameter

class GCodeParserTests1 extends FlatSpec with Matchers {
 
  "regexp test" should "work" in {
     val r = """(?:[ \t\r]+)|(?:\([^\)]*\))""".r.replaceAllIn("  h i(fan)h(keep h)(d s \ndfg ) a a", "")  
     r.size should be (5)
     r should be ("hihaa")
  }
  
  "gSingleLineParameters wo comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "x1 y2 z3"
    val parsed = objParser.parse(objParser.gSingleLineParameters,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val stepData = ret.parameters.toArray 
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }
  
  "gSingleLineParameters with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "x5 y6 (this is a comment)z7(this is too)"
    val parsed = objParser.parse(objParser.gSingleLineParameters,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val stepData = ret.parameters.toArray 
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (5f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (6f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (7f)
  }

  "gMultiLineCommand wo comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "g0  x1 y2 z3"
    val parsed = objParser.parse(objParser.gMultiLineCommand,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get match { case rv:GcodeMultiCommand => rv }
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val steps = ret.steps.toArray 
    steps.size should be (1)
    val stepData = steps(0).parameters.toArray
    ret.key should be ("G0")
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }
  
  "gMultiLineCommand with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "(comment)g0()  x4 y5 z6(comment)"
    val parsed = objParser.parse(objParser.gMultiLineCommand,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get match { case rv:GcodeMultiCommand => rv }
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val steps = ret.steps.toArray 
    steps.size should be (1)
    val stepData = steps(0).parameters.toArray
    ret.key should be ("G0")
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (4f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (5f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (6f)
  }
  
  "gMultiLineCommand multiline with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "n0g0  x1 y2  z3 (comme\nnt comment)\nx2 y2 (ffdf)z-3(dfgdf)"
    val parsed = objParser.parse(objParser.gMultiLineCommand,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get match { case rv:GcodeMultiCommand => rv }

    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val steps = ret.steps.toArray 
    steps.size should be (2)
    var stepData = steps(0).parameters.toArray
    ret.key should be ("G0")
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
    stepData = steps(1).parameters.toArray
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (2f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (-3f)
  }
  
  "gSingleLineCommand with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "(comment)G17 X0 i2j-1(comment)"
    val parsed = objParser.parse(objParser.gSingleLineCommand,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get match { case rv:GcodeSingleCommand => rv }
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val parameters = ret.parameters.get
    parameters.parameters.size should be (3)
    val stepData = parameters.parameters.toArray
    ret.key should be ("G17")
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (0f)
    stepData(1).key   should be ("I")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("J")
    stepData(2).value should be (-1f)
  }
  
  "gSingleLineCommand with G0" should "completely fail" in {
    val objParser = new GCodeParser
    val arg = "(comment)G0 X0 i2j-1(comment)"
    val parsed = objParser.parse(objParser.gSingleLineCommand,objParser.filterOutWhiteSpace(arg))
    parsed.successful should be (false)
  }
  
  "gSingleLineCommand with G00" should "completely fail" in {
    val objParser = new GCodeParser
    val arg = "(comment)G0 X0 i2j-1(comment)"
    val parsed = objParser.parse(objParser.gSingleLineCommand,objParser.filterOutWhiteSpace(arg))
    parsed.successful should be (false)
  }
  
  "gSingleLineCommand with G1" should "completely fail" in {
    val objParser = new GCodeParser
    val arg = "(comment)G1 X0 i2j-1(comment)"
    val parsed = objParser.parse(objParser.gSingleLineCommand,objParser.filterOutWhiteSpace(arg))
    parsed.successful should be (false)
  }
  
  "gSingleLineCommand with G01" should "completely fail" in {
    val objParser = new GCodeParser
    val arg = "(comment)G1 X0 i2j-1(comment)"
    val parsed = objParser.parse(objParser.gSingleLineCommand,objParser.filterOutWhiteSpace(arg))
    parsed.successful should be (false)
  }
   
  "gCode multiline with comment 2" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "    G17 G20 G40 G49 G54 G80 G90 G94\nN 0 g0  x1 y2  z3 (comme\nnt comment)\nx1 y1 z1\nN1 g1 x2 y2 z3"
    val parsed = objParser.parse(objParser.gCode,objParser.filterOutWhiteSpace(arg))
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    parsed.successful should be (true)
    
    val ret = parsed.get
    rest.size should be (0)
    ret.commands.size should be (10)
    
    var command = ret.commands(0) match { case c:GcodeSingleCommand => c}
    command.key should be ("G17")   
    command.parameters.size should be (0)
    
    command = ret.commands(1) match { case c:GcodeSingleCommand => c}
    command.key should be ("G20")   
    command.parameters.size should be (0)
    
    command = ret.commands(2) match { case c:GcodeSingleCommand => c}
    command.key should be ("G40")   
    command.parameters.size should be (0)
    
    command = ret.commands(3) match { case c:GcodeSingleCommand => c}
    command.key should be ("G49")   
    command.parameters.size should be (0)
    
    command = ret.commands(4) match { case c:GcodeSingleCommand => c}
    command.key should be ("G54")   
    command.parameters.size should be (0)
    
    command = ret.commands(5) match { case c:GcodeSingleCommand => c}
    command.key should be ("G80")   
    command.parameters.size should be (0)
    
    command = ret.commands(6) match { case c:GcodeSingleCommand => c}
    command.key should be ("G90")   
    command.parameters.size should be (0)
    
    command = ret.commands(7) match { case c:GcodeSingleCommand => c}
    command.key should be ("G94")   
    command.parameters.size should be (0)
    
    var command1 = ret.commands(8) match { case c:GcodeMultiCommand => c}
    command1.key should be ("G0")   
    command1.steps.size should be (2)
    var stepData = command1.steps(0).parameters
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
    
    command1 = ret.commands(9) match { case c:GcodeMultiCommand => c}
    command1.key should be ("G1")   
    command1.steps.size should be (1)
    stepData = command1.steps(0).parameters
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (2f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f) 
  }
 
  "gCode multiline with comment 3" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = """
  G0Z2.0
M3S1000
G4P3
G0F1000
G1F500
G64P0.02Q0.02
G0Z2.000000
      
G0X19.01426Y26.41212
G1X19.01426Y26.41212Z0F500.000000
G1X19.01426Y26.41212Z-11.68791F100.000000
X19.01426Y26.41212Z-11.68791F500.000000
G1X19.01426Y26.41212Z-11.68791"""
    val parsed = objParser.parse(objParser.gCode,objParser.filterOutWhiteSpace(arg))
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    parsed.successful should be (true)
    rest.size should be (0)
    val ret = parsed.get
    ret.commands.size should be (11)
  }  
}