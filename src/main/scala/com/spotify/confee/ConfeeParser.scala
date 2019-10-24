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

  /* ========== AST root ========== */

  /* ---- grammar ----- */

  def grammar: Parser[Grammar] = positioned {

    val a = stmt ~ grammar ^^ { case x ~ xs => Grammar(x :: xs.stmts) }

    val b = stmt ^^ { x => Grammar(x :: List.empty) }

    a | b
  }

  /* ========== AST non-terminals ========== */

  /* ---- statements ----- */

  def stmt: Parser[Stmt] = positioned {
    typeStmt | factStmt
  }

  /* ----- type statement ----- */

  def typeStmt: Parser[TypeStmt] = positioned {
    typeKeyword ~ name ~ braceOpen ~ typeStmtItems ~ braceClose ^^ {
      case _ ~ n ~ _ ~ ti ~ _ => TypeStmt(n, ti)
    }
  }

  def typeStmtItems: Parser[TypeItems] = positioned {

    val a = typeStmtItem ~ typeStmtItems ^^ { case x ~ xs => TypeItems(x :: xs.items) }

    val b = opt(typeStmtItem) ^^ {
      case Some(x) => TypeItems(x :: List.empty)
      case None => TypeItems(List.empty)
    }

    a | b
  }

  def typeStmtItem: Parser[TypeItem] = positioned {

    val a = word ~ colon ~ name ^^ {
      case w ~ _ ~ n => TypeItem(w, TypeDef(Left(n), isList = false))
    }

    val b = word ~ colon ~ bracketOpen ~ name ~ bracketClose ^^ {
      case w ~ _ ~ _ ~ n ~ _ => TypeItem(w, TypeDef(Left(n), isList = true))
    }

    a | b
  }

  /* ----- fact statement ----- */

  def factStmt: Parser[FactStmt] = positioned {
    factKeyword ~ word ~ colon ~ factStmtType ~ braceOpen ~ factStmtItems ~ braceClose ^^ {
      case _ ~ w ~ _ ~ t ~ _ ~ si ~ _ => FactStmt(w, t, FactItems(si.items))
    }
  }

  def factStmtItems: Parser[FactItems] = positioned {

    val a = factStmtItem ~ factStmtItems ^^ { case x ~ xs => FactItems(x :: xs.items) }

    val b = opt(factStmtItem) ^^ {
      case Some(x) => FactItems(x :: List.empty)
      case None => FactItems(List.empty)
    }

    a | b
  }

  def factStmtItem: Parser[FactItem] = positioned {
    word ~ assignment ~ literal ^^ { case w ~ _ ~ l => FactItem(w, l) }
  }

  def factStmtType: Parser[TypeDef] = positioned {

    val a = word ^^ { w => TypeDef(Right(w), isList = false) }

    val b = name ^^ { n => TypeDef(Left(n), isList = false) }

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

  def exprArithOperator: Parser[Node] = positioned {
    addition | subtraction | division | multiplication | modulus
  }

  /* ----- literal ----- */

  def literal: Parser[Literal] = positioned {
    val a = string ^^ { x => StringLiteral(x) }

    val b = number ^^ { x => NumberLiteral(x) }

    a | b
  }

  /* ========== AST terminals ========== */

  def string: Parser[StringToken] = positioned {
    accept("string", { case token@StringToken(_) => token })
  }

  def number: Parser[NumberToken] = positioned {
    accept("number", { case token@NumberToken(_) => token })
  }

  def typeKeyword: Parser[TypeKeywordToken] = positioned {
    accept("typeKeyword", { case token@TypeKeywordToken() => token })
  }

  def factKeyword: Parser[FactKeywordToken] = positioned {
    accept("factKeyword", { case token@FactKeywordToken() => token })
  }

  def word: Parser[WordToken] = positioned {
    accept("number", { case token@WordToken(_) => token })
  }

  def name: Parser[NameToken] = positioned {
    accept("name", { case token@NameToken(_) => token })
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

  def assignment: Parser[AssignmentToken] = positioned {
    accept("assignment", { case token@AssignmentToken() => token })
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
