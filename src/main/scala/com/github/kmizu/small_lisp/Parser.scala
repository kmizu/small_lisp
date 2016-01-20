package com.github.kmizu.small_lisp

import fastparse.all._

object Parser {
  abstract sealed class SExpression
  case class SList(parameters: List[SExpression]) extends SExpression
  abstract sealed class Atom extends SExpression
  case class IntValue(value: Int) extends Atom
  case class DoubleValue(value: Double) extends Atom
  case class StringValue(value: String) extends Atom
  case class BoolValue(value: Boolean) extends Atom
  case class Identifier(value: String) extends Atom

  val exp: P[SExpression] = P(list | atom)
  val list: P[SList] = P("(" ~ exp.rep(0) ~ ")").map(p => SList(p.toList))
  val atom: P[Atom] = P(integer | double | string | bool | identifier)
  val integer: P[IntValue] = ???
  val double: P[DoubleValue] = ???
  val string: P[StringValue] = ???
  val bool: P[BoolValue] = ???
  val identifier: P[Identifier] = ???
}