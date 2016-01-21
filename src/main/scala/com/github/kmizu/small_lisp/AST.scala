package com.github.kmizu.small_lisp

object AST {
  abstract sealed class SExpression
  case class SList(parameters: List[SExpression]) extends SExpression
  abstract sealed class Atom extends SExpression
  case class IntValue(value: Int) extends Atom
  case class DoubleValue(value: Double) extends Atom
  case class StringValue(value: String) extends Atom
  case class BoolValue(value: Boolean) extends Atom
  case class Identifier(value: String) extends Atom
}
