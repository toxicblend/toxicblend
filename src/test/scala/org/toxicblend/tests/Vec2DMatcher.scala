package org.toxicblend.tests

import org.scalatest._
import matchers._
import org.toxicblend.vecmath.Vec2D

trait Vec2DMatchers {

  class Vec2DMatcher(expectedValue:Vec2D, ε:Double) extends Matcher[Vec2D] {

    def apply(left: Vec2D) = {
      MatchResult(
        left.=~=(expectedValue,  ε),
        s"""$left does not equal $expectedValue""",
        s"""$left does equal $expectedValue"""
      )
    }
  }

  def equal2d(expectedValue:Vec2D, ε:Double) = new Vec2DMatcher(expectedValue, ε)
}

// Make them easy to import with:
// import Vec2DMatcher._
object Vec2DMatcher extends Vec2DMatchers