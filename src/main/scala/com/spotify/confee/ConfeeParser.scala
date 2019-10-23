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
    grammar(reader) match {
      case NoSuccess(msg, next) =>
        Left(ConfeeParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) =>
        Right(result)
    }
  }

  /* ========== non-terminals ========== */

  /* ---- grammar ----- */

  def grammar: Parser[Grammar] = positioned {

    val a = stmt ~ grammar ^^ { case x ~ xs => Grammar(x :: xs.stmts)  }

    val b = stmt ^^ { x => Grammar(x :: List()) }

    a | b
  }

  /* ---- statements ----- */

  def stmt: Parser[Stmt] = positioned { typeStmt | factStmt }

  /* ----- type statement ----- */

  def typeStmt: Parser[TypeStmt] = positioned {
    keyword ~ name ~ braceOpen ~ braceClose ^^ {
      case k ~ n ~ bo ~ bc => TypeStmt(n, List())
    }
  }

  /* ----- fact statement ----- */

  def factStmt: Parser[FactStmt] = positioned {
    keyword ~ word ~ colon ~ factStmtType ~ braceOpen ~ braceClose ^^ {
      case k ~ w ~ c ~ t ~ bo ~ bc => FactStmt(w, t, List())
    }
  }

  def factStmtType: Parser[TypeDef] = positioned {
    val a = word ^^ { w => TypeDef(Right(w), isList = false) }

    val b = name ^^ { n => TypeDef(Left(n), isList = false )}

    a | b
  }

  /* ----- arithmetic expression ----- */

  def exprArith: Parser[ConfeeAST] = positioned {
    val a = exprArithFactor ~ exprArithOperator ~ exprArith ^^ {
      case x ~ op ~ y => DebuggingStmt("expr-arith-a", List(x, op, y))
    }

    val b = exprArithFactor ~ exprArithOperator ~ exprArithFactor ^^ {
      case x ~ op ~ y => DebuggingStmt("expr-arith-b", List(x, op, y))
    }

    val c = exprArithFactor ^^ (x => DebuggingStmt("expr-arith-b", List(x)))

    a | b | c
  }

  def exprArithFactor: Parser[ConfeeAST] = positioned {
    val a = parenthesesOpen ~ exprArith ~ parenthesesClose ^^ {
      case x ~ y ~ z => DebuggingStmt("expr-arith-factor-a", List(x, y, z))
    }

    val b = number ^^ (n => DebuggingStmt("expr-arith-factor-b", List(n)))

    a | b
  }

  def exprArithOperator: Parser[ConfeeToken] = positioned {
    addition | subtraction | division | multiplication | modulus
  }

  /* ========== terminals ========== */

  def string: Parser[StringToken] = positioned {
    accept("string", { case token@StringToken(_) => token })
  }

  def number: Parser[NumberToken] = positioned {
    accept("number", { case token@NumberToken(_) => token })
  }

  def word: Parser[WordToken] = positioned {
    accept("number", { case token@WordToken(_) => token })
  }

  def name: Parser[NameToken] = positioned {
    accept("name", { case token@NameToken(_) => token })
  }

  def keyword: Parser[KeywordToken] = positioned {
    accept("keyword", { case token@KeywordToken(_) => token })
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

  def parenthesesOpen: Parser[ParenthesesOpenToken] = positioned {
    accept("parenthesesOpen", { case token@ParenthesesOpenToken() => token })
  }

  def parenthesesClose: Parser[ParenthesesCloseToken] = positioned {
    accept("parenthesesClose", { case token@ParenthesesCloseToken() => token })
  }

  def bracketOpen: Parser[BracketOpenToken] = positioned {
    accept("bracketOpen", { case token@BracketOpenToken() => token })
  }

  def bracketClose: Parser[BracketCloseToken] = positioned {
    accept("bracketClose", { case token@BracketCloseToken() => token })
  }

  def braceOpen: Parser[BraceOpenToken] = positioned {
    accept("braceOpen", { case token@BraceOpenToken() => token })
  }

  def braceClose: Parser[BraceCloseToken] = positioned {
    accept("braceClose", { case token@BraceCloseToken() => token })
  }

  def separator: Parser[SeparatorToken] = positioned {
    accept("separator", { case token@SeparatorToken() => token })
  }

  def colon: Parser[ColonToken] = positioned {
    accept("colon", { case token@ColonToken() => token })
  }

  def semiColon: Parser[SemiColonToken] = positioned {
    accept("semiColon", { case token@SemiColonToken() => token })
  }

  def hash: Parser[HashToken] = positioned {
    accept("hash", { case token@HashToken() => token })
  }

  def dot: Parser[DotToken] = positioned {
    accept("dot", { case token@DotToken() => token })
  }

}
