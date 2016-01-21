package com.github.kmizu.small_lisp

import fastparse.all._

object SExpressionParser {
  abstract sealed class SExpression
  case class SList(parameters: List[SExpression]) extends SExpression
  abstract sealed class Atom extends SExpression
  case class IntValue(value: Int) extends Atom
  case class DoubleValue(value: Double) extends Atom
  case class StringValue(value: String) extends Atom
  case class BoolValue(value: Boolean) extends Atom
  case class Identifier(value: String) extends Atom


  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
  val chars         = P(CharsWhile(!"\"\\".contains(_: Char)))

  val expression: P[SExpression] = P(list | atom)
  val list: P[SList] = P("(" ~ expression.rep(0) ~ ")").map(p => SList(p.toList))
  val atom: P[Atom] = P(integer | double | string | bool | identifier)
  val integer: P[IntValue] = P(CharIn('0'to'9').rep(1).!.map{v => IntValue(v.toInt)})
  val double: P[DoubleValue] = P((CharIn('0'to'9').rep(1) ~ "." ~ CharIn('0'to'9').rep(1)).!.map{v => DoubleValue(v.toDouble)})
  val string: P[StringValue] = P("\"" ~ (chars | escape).rep(0).! ~ "\"").map(StringValue(_))
  val bool: P[BoolValue] = P(("true".!.map{_ => true} | "false".!.map{_ => false}).map{BoolValue(_)})
  val identifier: P[Identifier] = P{
    (identifierFirst ~ (identifierFirst | P(CharIn('0'to'9'))).rep(0)).!.map{Identifier(_)}
  }
  val identifierFirst: P[String] = P {
    CharIn('a' to 'z', 'A' to 'Z', List('_', '-', '+', '*', '/', '#', '$', '&', '!', '?', '%', '|', '=', '^', ':')).!
  }
}