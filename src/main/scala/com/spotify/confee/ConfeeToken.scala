package com.spotify.confee

import scala.util.parsing.input.Positional

sealed trait ConfeeToken extends Positional

/* string and number */

case class StringToken(value: String) extends ConfeeToken

case class NumberToken(value: Double) extends ConfeeToken

/* identifiers */

case class WordToken(word: String) extends ConfeeToken

case class NameToken(name: String) extends ConfeeToken

case class KeywordToken(keyword: String) extends ConfeeToken

/* arithmetic and assignment operators */

case class AdditionToken() extends ConfeeToken

case class SubtractionToken() extends ConfeeToken

case class DivisionToken() extends ConfeeToken

case class MultiplicationToken() extends ConfeeToken

case class ModulusToken() extends ConfeeToken

case class AssignmentToken() extends ConfeeToken

/* delimiter and grouping operators */

case class ParenthesesOpenToken() extends ConfeeToken

case class ParenthesesCloseToken() extends ConfeeToken

case class BracketOpenToken() extends ConfeeToken

case class BracketCloseToken() extends ConfeeToken

case class BraceOpenToken() extends ConfeeToken

case class BraceCloseToken() extends ConfeeToken

case class SeparatorToken() extends ConfeeToken

case class ColonToken() extends ConfeeToken

case class SemiColonToken() extends ConfeeToken

case class HashToken() extends ConfeeToken

case class DotToken() extends ConfeeToken



