package com.github.kmizu.small_lisp

import fastparse.core.Parsed
import org.scalatest.FunSpec
import SExpressionParser.expression
import AST._
import fastparse.all._

class SExpressionParserSpec extends FunSpec {
  describe("Parse integer") {
    it("0, 1, 2, 3") {
      assert(expression.parse("0").get.value == IntValue(0))
      assert(expression.parse("1").get.value == IntValue(1))
      assert(expression.parse("2").get.value == IntValue(2))
      assert(expression.parse("3").get.value == IntValue(3))
    }
    it("00, 01, 02, 03") {
      assert(expression.parse("00").get.value == IntValue(0))
      assert(expression.parse("01").get.value == IntValue(1))
      assert(expression.parse("02").get.value == IntValue(2))
      assert(expression.parse("03").get.value == IntValue(3))
    }
    it("-0, -1, -2, -3") {
      assert(expression.parse("-0").get.value == IntValue(0))
      assert(expression.parse("-1").get.value == IntValue(-1))
      assert(expression.parse("-2").get.value == IntValue(-2))
      assert(expression.parse("-3").get.value == IntValue(-3))
    }
  }
}
