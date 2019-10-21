package com.spotify.confee

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object ConfeeParser extends Parsers {

  override type Elem = ConfeeToken

  class ConfeeTokenReader(tokens: Seq[ConfeeToken]) extends Reader[ConfeeToken] {
    override def first: ConfeeToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[ConfeeToken] = new ConfeeTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[ConfeeToken]): Either[ConfeeParserError, ConfeeAST] = {
    val reader: ConfeeTokenReader = new ConfeeTokenReader(tokens)
    exprArith(reader) match {
      case NoSuccess(msg, next) =>
        Left(ConfeeParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) =>
        Right(result)
    }
  }

  /* non-terminals */

  def exprArith: Parser[ConfeeAST] = positioned {
    val a = exprArithFactor ~ exprArithOperator ~ exprArith ^^ {
      case x ~ op ~ y => Stmt("as-exp", List(x, op, y))
    }

    val b = exprArithFactor ~ exprArithOperator ~ exprArithFactor ^^ {
      case x ~ op ~ y => Stmt("a-expr", List(x, op, y))
    }

    val c = exprArithFactor ^^ (x => Stmt("n-expr", List(x)))

    a | b | c
  }

  def exprArithFactor: Parser[ConfeeAST] = positioned {
    val a = openParentheses ~ exprArith ~ closeParentheses ^^ {
      case x ~ y ~ z => Stmt("p-expr", List(x, y, z))
    }

    val b = number ^^ (n => Stmt("f-expr", List(n)))

    a | b
  }

  def exprArithOperator: Parser[ConfeeToken] = positioned {
    addition | subtraction | division | multiplication | modulus
  }

  /* terminals */

  def number: Parser[NumberToken] = positioned {
    accept("number", { case token@NumberToken(_) => token })
  }

  def addition: Parser[AdditionToken] = positioned {
    accept("addition", { case token@AdditionToken() => token })
  }

  def subtraction: Parser[SubtractionToken] = positioned {
    accept("subtraction", { case token@SubtractionToken() => token })
  }

  def division: Parser[DivisionToken] = positioned {
    accept("division", { case token@DivisionToken() => token })
  }

  def multiplication: Parser[MultiplicationToken] = positioned {
    accept("multiplication", { case token@MultiplicationToken() => token })
  }

  def modulus: Parser[ModulusToken] = positioned {
    accept("modulus", { case token@ModulusToken() => token })
  }

  def openParentheses: Parser[ParenthesesOpenToken] = positioned {
    accept("parenthesesOpen", { case token@ParenthesesOpenToken() => token })
  }

  def closeParentheses: Parser[ParenthesesCloseToken] = positioned {
    accept("parenthesesClose", { case token@ParenthesesCloseToken() => token })
  }

}
