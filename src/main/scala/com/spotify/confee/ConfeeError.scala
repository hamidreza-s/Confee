package com.spotify.confee

sealed trait ConfeeError

case class ConfeeLexerError(location: Location, msg: String) extends ConfeeError
case class ConfeeParserError(location: Location, msg: String) extends ConfeeError
case class ConfeeEvaluatorError(location: Location, msg: String) extends ConfeeError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
