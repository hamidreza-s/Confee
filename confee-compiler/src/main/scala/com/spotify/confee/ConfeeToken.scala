package com.spotify.confee

import scala.util.parsing.input.Positional

sealed trait ConfeeToken extends Positional with Node

/* string and number */

case class StringToken(value: String) extends ConfeeToken

case class NumberToken(value: Double) extends ConfeeToken

/* boolean */

case class BoolToken(value: Boolean) extends ConfeeToken

/* keywords */

case class KeywordToken(keyword: String) extends ConfeeToken

case class TypeKeywordToken() extends ConfeeToken

case class ConfKeywordToken() extends ConfeeToken

case class ImportKeywordToken() extends ConfeeToken

/* identifiers */

case class WordToken(word: String) extends ConfeeToken // starts with lowercase

case class NameToken(name: String) extends ConfeeToken // starts with uppercase

/* bitwise operators */

case class NotToken() extends ConfeeToken

case class AndToken() extends ConfeeToken

case class OrToken() extends ConfeeToken

case class XorToken() extends ConfeeToken

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
