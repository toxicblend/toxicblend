package org.toxicblend.tests

import toxi.geom.Vec3D
import org.scalatest._
import org.toxicblend.operations.simplegcodegenerator.GCode
import org.toxicblend.operations.simplegcodegenerator.GCodeState
import org.toxicblend.operations.simplegcodegenerator.GCodeSettings

class GCodeTests1 extends FlatSpec with Matchers {
  def setup():GCodeSettings = {
    val rv = new GCodeSettings(outFilename="test.ngc",safeZ=2.f, g0Feedrate=1, g1Feedrate=2,
                               g1PlungeFeedrate=3,spindleSpeed=4,g64Command="G64",
                               customEndCommand="M101",stepDown=5f)
    rv
  }
  
  "gCodeSlow 1" should "trim away redundant information" in {
    val prop = setup
    val g = new GCode(new Array[Vec3D](0))
    var state:Option[GCodeState] = None
    state = {
      val p = new Vec3D(0,0,0)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("G1 X0 Y0 Z0 F2")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(0,0,0)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(10,0,0)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("X10")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(1,1,0)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("X1 Y1")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(2,2,2)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("X2 Y2 Z2")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val prop2 = new GCodeSettings(outFilename=prop.outFilename, safeZ=prop.safeZ, g0Feedrate=prop.g0Feedrate, g1Feedrate=10,
                                g1PlungeFeedrate=prop.g1PlungeFeedrate, spindleSpeed=prop.spindleSpeed,
                                g64Command=prop.g64Command, customEndCommand=prop.customEndCommand,
                                stepDown=prop.stepDown) 
      val p = new Vec3D(2,0,2)
      val (state0,text0) = g.gCodeSlow(state, p, prop2)
      text0.trim() should be ("Y0 F10")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(1,2,3)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("X1 Y2 Z3 F2")
      state0.isDefined should be (true)
      state0
    }
  }
  
  "gCodeFastSafeZ 1" should "trim away redundant information" in {
    val prop = setup
    val g = new GCode(new Array[Vec3D](0))
    var state:Option[GCodeState] = None
    state = {
      val p = new Vec3D(0,0,0)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("G1 X0 Y0 Z0 F2")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(0,0,0)
      val (state0,text0) = g.gCodeFastSafeZ(state, prop)
      text0.trim() should be ("G0 Z2 F1")
      state0
    }
    state = {
      val prop2 = new GCodeSettings(outFilename=prop.outFilename, safeZ=5, g0Feedrate=123, 
                                    g1Feedrate=prop.g1Feedrate,
                                    g1PlungeFeedrate=prop.g1PlungeFeedrate, spindleSpeed=prop.spindleSpeed,
                                    g64Command=prop.g64Command, customEndCommand=prop.customEndCommand,
                                    stepDown=prop.stepDown) 
      val (state0,text0) = g.gCodeFastSafeZ(state, prop2)
      text0.trim() should be ("Z5 F123")
      state0
    }
    state = {
      val (state0,text0) = g.gCodeFastSafeZ(state, prop)
      text0.trim() should be ("Z2 F1")
      state0
    }
  }
  
   "gCodeFastXY 1" should "trim away redundant information" in {
    val prop = setup
    val g = new GCode(new Array[Vec3D](0))
    var state:Option[GCodeState] = None
    state = {
      val p = new Vec3D(0,0,0)
      val (state0,text0) = g.gCodeFastXY(state, p, prop)
      text0.trim() should be ("G0 X0 Y0 F1")
      state0.isDefined should be (false)
      state0
    }
    state = {
      val p = new Vec3D(10,0,0)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("G1 X10 Y0 Z0 F2")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(1,1,1)
      val (state0,text0) = g.gCodeFastXY(state, p, prop)
      text0.trim() should be ("G0 X1 Y1 F1")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val p = new Vec3D(2,2,1)
      val (state0,text0) = g.gCodeFastXY(state, p, prop)
      text0.trim() should be ("X2 Y2")
      state0.isDefined should be (true)
      state0
    }
    state = {
      val prop2 = new GCodeSettings(outFilename=prop.outFilename, safeZ=prop.safeZ, g0Feedrate=123, 
                                    g1Feedrate=prop.g1Feedrate,
                                    g1PlungeFeedrate=prop.g1PlungeFeedrate, spindleSpeed=prop.spindleSpeed,
                                    g64Command=prop.g64Command, customEndCommand=prop.customEndCommand,
                                    stepDown=prop.stepDown) 
      val p = new Vec3D(2,3,1)
      val (state0,text0) = g.gCodeFastXY(state, p, prop2)
      text0.trim() should be ("Y3 F123")
      state0
    }
    state = {
      val p = new Vec3D(3,3,1)
      val (state0,text0) = g.gCodeFastXY(state, p, prop)
      text0.trim() should be ("X3 F1")
      state0
    }
  }
   
  "gCodePlunge 1" should "trim away redundant information" in {
    val prop = setup
    val g = new GCode(new Array[Vec3D](0))
    var state:Option[GCodeState] = None
    state = {
      val p = new Vec3D(0,0,0)
      val (state0,text0) = g.gCodeSlow(state, p, prop)
      text0.trim() should be ("G1 X0 Y0 Z0 F2")
      state0
    }
    state = {
      val zPos= -3f
      val (state0,text0) = g.gCodePlunge(state, zPos, prop)
      val texta = text0.split("\n\r?")
      texta.size should be (2)
      state0.isDefined should be (true)
      texta(0).trim() should be ("Z0")
      texta(1).trim() should be ("Z-3 F3")
      state0
    }
    state = {
      val p = new Vec3D(2,2,1)
      val (state0,text0) = g.gCodeFastXY(state, p, prop)
      text0.trim() should be ("G0 X2 Y2 F1")
      state0.isDefined should be (true)
      state0
    }
    
    state = {
      val prop2 = new GCodeSettings(outFilename=prop.outFilename, safeZ=prop.safeZ, g0Feedrate=prop.g0Feedrate, g1Feedrate=10,
                                g1PlungeFeedrate=prop.g1PlungeFeedrate, spindleSpeed=prop.spindleSpeed,
                                g64Command=prop.g64Command, customEndCommand=prop.customEndCommand,
                                stepDown=prop.stepDown) 
      val zPos= 0f
      val (state0,text0) = g.gCodePlunge(state, zPos, prop2)
      val texta = text0.split("\n\r?")
      texta.size should be (1)
      texta(0).trim() should be ("G1 Z0 F10")
      state0
    }
    
    state = {
      val zPos= -3f
      val (state0,text0) = g.gCodePlunge(state, zPos, prop)
      val texta = text0.split("\n\r?")
      texta.size should be (2)
      state0.isDefined should be (true)
      texta(0).trim() should be ("Z0 F2")
      texta(1).trim() should be ("Z-3 F3")
      state0
    }
  }
}