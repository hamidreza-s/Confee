package com.spotify.confee

sealed trait ConfeeError

case class ConfeeLexerError(location: Location, msg: String) extends ConfeeError

case class ConfeeParserError(location: Location, msg: String) extends ConfeeError

case class ConfeeBinderError(location: Location, msg: String) extends ConfeeError

case class ConfeeEvaluatorError(location: Location, msg: String) extends ConfeeError

case class ConfeeUnknownError(exception: Throwable) extends ConfeeError

case class ConfeeException(location: Location, msg: String) extends Exception(msg)

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
