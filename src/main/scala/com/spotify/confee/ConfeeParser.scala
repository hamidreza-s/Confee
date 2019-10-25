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
    word ~ assignment ~ expr ^^ { case w ~ _ ~ e => FactItem(w, e) }
  }

  def factStmtType: Parser[TypeDef] = positioned {

    val a = word ^^ { w => TypeDef(Right(w), isList = false) }

    val b = name ^^ { n => TypeDef(Left(n), isList = false) }

    a | b
  }

  /* ----- expression ----- */

  def expr: Parser[Expr] = positioned {
    exprLiteral
  }

  /* ----- literal expression ----- */

  def exprLiteral: Parser[LiteralExpr] = positioned {

    val a = string ^^ { x => LiteralString(x) }

    val b = exprLiteralNumber ^^ { x => x }

    val c = listLiteral ^^ { x => x }

    a | b | c
  }

  /* ----- literal number expression ----- */

  def exprLiteralNumber: Parser[LiteralNumber] = positioned {

    val a = exprLiteralNumberFactor ~ exprLiteralNumberOperator ~ exprLiteralNumber ^^ {
      case x ~ op ~ xs => LiteralNumberGroup(op, x, xs)
    }

    val b = exprLiteralNumberFactor ~ exprLiteralNumberOperator ~ exprLiteralNumberFactor ^^ {
      case x ~ op ~ y => LiteralNumberGroup(op, x, y)
    }

    val c = exprLiteralNumberFactor ^^ { f => f }

    a | b | c
  }


  def exprLiteralNumberFactor: Parser[LiteralNumber] = positioned {

    val a = parenthesesOpen ~ exprLiteralNumber ~ parenthesesClose ^^ { case _ ~ e ~ _ => e }

    val b = number ^^ { n => LiteralNumberFactor(n) }

    val c = word ^^ { n => LiteralNumberWord(n) }

    a | b | c
  }


  def exprLiteralNumberOperator: Parser[LiteralNumberOperator] = positioned {

    val a = addition ^^ { _ => ArithAddOperator() }

    val b = subtraction ^^ { _ => ArithSubOperator() }

    val c = division ^^ { _ => ArithDivOperator() }

    val d = multiplication ^^ { _ => ArithMulOperator() }

    val e = modulus ^^ { _ => ArithModOperator() }

    a | b | c | d | e
  }

  /* ----- literal list expression ----- */

  def listLiteral: Parser[LiteralList] = positioned {
    bracketOpen ~ listLiteralItems ~ bracketClose ^^ { case _ ~ li ~ _ => LiteralList(li.value) }
  }

  def listLiteralItems: Parser[LiteralList] = positioned {

    val a = listLiteralItem ~ separator ~ listLiteralItems ^^ {
      case x ~_ ~  xs => LiteralList(x :: xs.value)
    }

    val b = opt(listLiteralItem) ^^ {
      case Some(x) => LiteralList(x :: List.empty)
      case None => LiteralList(List.empty)
    }

    a | b
  }

  def listLiteralItem: Parser[LiteralExpr] = positioned {
    exprLiteral | listLiteral
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
