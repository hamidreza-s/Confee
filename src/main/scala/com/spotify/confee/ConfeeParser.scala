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

  def exprArith: Parser[ConfeeAST] = positioned {
    val a = exprArithFactor ~ operator ~ exprArith ^^ {
      case x ~ op ~ y => Stmt("as-exp", List(x, op, y))
    }

    val b = exprArithFactor ~ operator ~ exprArithFactor ^^ {
      case x ~ op ~ y => Stmt("a-expr", List(x, op, y))
    }

    val c = exprArithFactor ^^ (x => Stmt("n-expr", List(x)))

    a | b | c
  }

  def exprArithFactor: Parser[ConfeeAST] = positioned {
    val a = punctuation ~ exprArith ~ punctuation ^^ {
      case x ~ y ~ z => Stmt("p-expr", List(x, y, z))
    }

    val b = number ^^ (n => Stmt("f-expr", List(n)))

    a | b
  }

  def operator: Parser[OperatorConfeeToken] = positioned {
    accept("operator", { case token@OperatorConfeeToken(_) => token })
  }

  def number: Parser[NumberConfeeToken] = positioned {
    accept("number", { case token@NumberConfeeToken(_) => token })
  }

  def punctuation: Parser[PunctuationConfeeToken] = positioned {
    accept("punctuation", { case token@PunctuationConfeeToken(_) => token })
  }

}
