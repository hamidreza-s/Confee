package com.spotify.confee

import scala.util.parsing.input.Positional

sealed trait ConfeeToken extends Positional

case class StringConfeeToken(value: String) extends ConfeeToken
case class NumberConfeeToken(value: Double) extends ConfeeToken
case class NameConfeeToken(name: String) extends ConfeeToken
case class KeywordConfeeToken(keyword: String) extends ConfeeToken
case class PunctuationConfeeToken(punctuation: String) extends ConfeeToken
case class OperatorConfeeToken(operator: String) extends ConfeeToken

