package com.spotify.confee

import scala.util.parsing.input.Position

sealed trait ConfeeError

case class ConfeeLexerError(location: Location, msg: String) extends ConfeeError

case class ConfeeParserError(location: Location, msg: String) extends ConfeeError

case class ConfeeValidatorError(location: Location, msg: String) extends ConfeeError

case class ConfeeValidatorErrors(list: List[ConfeeValidatorError]) extends ConfeeError

case class ConfeeBinderError(location: Location, msg: String) extends ConfeeError

case class ConfeeEvaluatorError(location: Location, msg: String) extends ConfeeError

case class ConfeeConstructorError(location: Location, msg: String) extends ConfeeError

case class ConfeeFormatterError(location: Location, msg: String) extends ConfeeError

case class ConfeeUnknownError(exception: Throwable) extends ConfeeError

case class ConfeeNotImplementedError(msg: String) extends ConfeeError

case class ConfeeNotFoundError(msg: String) extends ConfeeError

case class ConfeeInvalidArgumentError(msg: String) extends ConfeeError

case class ConfeeIllegalOperationError(msg: String) extends ConfeeError

case class ConfeeCodeException(location: Location, msg: String) extends Exception(msg)

case class ConfeeException(msg: String) extends Exception(msg)

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

object Location {
  def fromPosition(pos: Position): Location = Location(pos.line, pos.column)
}
