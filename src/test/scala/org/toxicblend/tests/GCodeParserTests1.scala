package org.toxicblend.tests

import collection.mutable.Stack
import org.scalatest._
import org.toxicblend.operations.gcodeparse.GCodeParser
import org.toxicblend.operations.gcodeparse.GcodeCommand
import org.toxicblend.operations.gcodeparse.GcodeCommands
import org.toxicblend.operations.gcodeparse.GcodeStepArgument

class GCodeParserTests1 extends FlatSpec with Matchers {
  
  "regexp test" should "work" in {
     val r = """(?:[ \t\r]+)|(?:\([^\)]*\))""".r.replaceAllIn("  h i(fan)h(keep h)(d s \ndfg ) a a", "")  
     r.size should be (5)
     r should be ("hihaa")
  }
  
  "simple line wo comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "x1 y2 z3"
    val parsed = objParser.parse(objParser.gcodeSteps,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val steps = ret.toArray 
    steps.size should be (1)
    val stepData = steps(0).stepData.toArray
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }

  "simple line with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "x1 y2 z3 (comment1)"
    val parsed = objParser.parse(objParser.gcodeSteps,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val steps = ret.toArray 
    steps.size should be (1)
    val stepData = steps(0).stepData.toArray
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }
  
  "simple gcode line wo comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "g0  x1 y2 z3"
    val parsed = objParser.parse(objParser.gcodeCommand,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    
    val steps = ret.steps 
    steps.size should be (1)
    val stepData = steps(0).stepData.toArray
    ret.key should be ("G0")
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }
  
  "simple gcode line with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "g0  x1 y2  z3 (comme\nnt comment)"
    val parsed = objParser.parse(objParser.gcodeCommand,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    val steps = ret.steps 
    steps.size should be (1)
    val stepData = steps(0).stepData.toArray
    ret.key should be ("G0")
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }
  
  "gcode multiline with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "g0  x1 y2  z3 (comme\nnt comment)\nx2 y2 z3"
    val parsed = objParser.parse(objParser.gcodeCommand,objParser.filterOutWhiteSpace(arg))
    val ret = parsed.get
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    rest.size should be (0)
    val steps = ret.steps 
    steps.size should be (2)
    var stepData = steps(0).stepData.toArray
    ret.key should be ("G0")
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
    stepData = steps(1).stepData.toArray
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (2f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }
  
  "multigcode multiline with comment" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "n0g0  x1 y2  z3 (comme\nnt comment)\nN1 g1 x2 y2 z3"
    val arg1 = objParser.filterOutWhiteSpace(arg)
    val parsed = objParser.parse(objParser.gcodeCommands,arg1)
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    parsed.successful should be (true)
    val ret = parsed.get
    rest.size should be (0)
    ret.commands.size should be (2)
    
    var command = ret.commands(0)
    var steps = command.steps
    var stepData = steps(0).stepData.toArray
    command.key should be ("G0")    
    steps.size should be (1)
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
    
    command = ret.commands(1)
    steps = command.steps
    stepData = steps(0).stepData.toArray
    
    stepData.size should be (3)
    steps.size should be (1)
    stepData(0).key   should be ("X")
    stepData(0).value should be (2f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }
  
  "multigcode multiline with comment 2" should "completely parse" in {
    val objParser = new GCodeParser
    val arg = "    G17 G20 G40 G49 G54 G80 G90 G94\nN 0 g0  x1 y2  z3 (comme\nnt comment)\nx1 y1 z1\nN1 g1 x2 y2 z3"
    val parsed = objParser.parse(objParser.gcodeCommands,objParser.filterOutWhiteSpace(arg))
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    parsed.successful should be (true)
    
    val ret = parsed.get
    rest.size should be (0)

    ret.commands.size should be (2)
    
    var command = ret.commands(0)
    var steps = command.steps
    var stepData = steps(0).stepData.toArray
    command.key should be ("G0")    
    steps.size should be (2)
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
    
    stepData = steps(1).stepData.toArray
    stepData.size should be (3)
    stepData(0).key   should be ("X")
    stepData(0).value should be (1f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (1f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (1f)
    
    command = ret.commands(1)
    steps = command.steps
    stepData = steps(0).stepData.toArray
    
    stepData.size should be (3)
    steps.size should be (1)
    stepData(0).key   should be ("X")
    stepData(0).value should be (2f)
    stepData(1).key   should be ("Y")
    stepData(1).value should be (2f)
    stepData(2).key   should be ("Z")
    stepData(2).value should be (3f)
  }

  "multigcode multiline with comment 3" should "completely parse" in {
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
G1X19.01426Y26.41212Z-11.68791F500.000000
G1X19.01426Y26.41212Z-11.68791"""
    val parsed = objParser.parse(objParser.gcodeCommands,objParser.filterOutWhiteSpace(arg))
    val rest = parsed.next.source.toString.substring(parsed.next.offset)
    if (rest.size > 0) println("unparsed:" + rest)
    parsed.successful should be (true)
    rest.size should be (0)
  }  
}