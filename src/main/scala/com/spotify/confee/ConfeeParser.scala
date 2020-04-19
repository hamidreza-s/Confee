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
    typeStmt | confStmt | importStmt
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

  /* ----- conf statement ----- */

  def confStmt: Parser[ConfStmt] = positioned {
    confKeyword ~ word ~ colon ~ confStmtType ~ braceOpen ~ confStmtItems ~ braceClose ^^ {
      case _ ~ w ~ _ ~ t ~ _ ~ si ~ _ => ConfStmt(w, t, ConfItems(si.items))
    }
  }

  def confStmtItems: Parser[ConfItems] = positioned {

    val a = confStmtItem ~ confStmtItems ^^ { case x ~ xs => ConfItems(x :: xs.items) }

    val b = opt(confStmtItem) ^^ {
      case Some(x) => ConfItems(x :: List.empty)
      case None => ConfItems(List.empty)
    }

    a | b
  }

  def confStmtItem: Parser[ConfItem] = positioned {
    word ~ assignment ~ expr ^^ { case w ~ _ ~ e => ConfItem(w, e) }
  }

  def confStmtType: Parser[TypeDef] = positioned {

    val a = word ^^ { w => TypeDef(Right(w), isList = false) }

    val b = name ^^ { n => TypeDef(Left(n), isList = false) }

    a | b
  }

  /* ----- import statement ----- */

  def importStmt: Parser[ImportStmt] = positioned {
    importKeyword ~ string ^^ { case _ ~ s => ImportStmt(s) }
  }

  /* ----- expression ----- */

  def expr: Parser[Expr] = positioned {
    exprLiteral
  }

  /* ----- literal expression ----- */

  def exprLiteral: Parser[LiteralExpr] = positioned {
    exprLiteralBool ||| exprLiteralString ||| exprLiteralNumber |||
    exprLiteralArray ||| exprLiteralObject ||| exprLiteralProto
  }

  /* ----- literal boolean expression ----- */

  def exprLiteralBool: Parser[LiteralBool] = positioned {
    val a = trueBool ^^ { _ => LiteralBoolTrue() }

    val b = falseBool ^^ { _ => LiteralBoolFalse() }

    a | b
  }

  /* ----- literal string expression ----- */

  def exprLiteralString: Parser[LiteralString] = positioned {

    val a = exprLiteralStringFactor ~ exprLiteralStringOperator ~ exprLiteralString ^^ {
      case x ~ op ~ xs => LiteralStringGroup(op, x, xs)
    }

    val b = exprLiteralStringFactor ~ exprLiteralStringOperator ~ exprLiteralStringFactor ^^ {
      case x ~ op ~ y => LiteralStringGroup(op, x, y)
    }

    val c = exprLiteralStringFactor ^^ { f => f }

    a | b | c
  }

  def exprLiteralStringFactor: Parser[LiteralString] = positioned {

    val a = parenthesesOpen ~ exprLiteralString ~ parenthesesClose ^^ { case _ ~ e ~ _ => e }

    val b = string ^^ { n => LiteralStringFactor(n) }

    val c = word ^^ { n => LiteralStringWord(n) }

    a | b | c
  }

  def exprLiteralStringOperator: Parser[LiteralStringOperator] = positioned {

    val a = addition ^^ { _ => LiteralStringOperatorConcat() }

    val b = subtraction ^^ { _ => LiteralStringOperatorRemove() }

    a | b
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

    val a = addition ^^ { _ => LiteralNumberOperatorAdd() }

    val b = subtraction ^^ { _ => LiteralNumberOperatorSub() }

    val c = division ^^ { _ => LiteralNumberOperatorDiv() }

    val d = multiplication ^^ { _ => LiteralNumberOperatorMul() }

    val e = modulus ^^ { _ => LiteralNumberOperatorMod() }

    a | b | c | d | e
  }

  /* ----- literal array expression ----- */

  def exprLiteralArray: Parser[LiteralArray] = positioned {
    bracketOpen ~ exprLiteralArrayItems ~ bracketClose ^^ {
      case _ ~ li ~ _ => LiteralArray(li.value)
    }
  }

  def exprLiteralArrayItems: Parser[LiteralArray] = positioned {

    val a = exprLiteralArrayItem ~ separator ~ exprLiteralArrayItems ^^ {
      case x ~ _ ~  xs => LiteralArray(x :: xs.value)
    }

    val b = opt(exprLiteralArrayItem) ^^ {
      case Some(x) => LiteralArray(x :: List.empty)
      case None => LiteralArray(List.empty)
    }

    a | b
  }

  def exprLiteralArrayItem: Parser[LiteralExpr] = positioned {
    exprLiteral
  }

  /* ----- literal object expression ----- */

  def exprLiteralObject: Parser[LiteralObject] = positioned {
    braceOpen ~ exprLiteralObjectItems ~ braceClose ^^ {
      case _ ~ si ~ _ => LiteralObject(LiteralObjectItems(si.items))
    }
  }

  def exprLiteralObjectItems: Parser[LiteralObjectItems] = positioned {

    val a = exprLiteralObjectItem ~ exprLiteralObjectItems ^^ {
      case x ~ xs => LiteralObjectItems(x :: xs.items)
    }

    val b = opt(exprLiteralObjectItem) ^^ {
      case Some(x) => LiteralObjectItems(x :: List.empty)
      case None => LiteralObjectItems(List.empty)
    }

    a | b
  }

  def exprLiteralObjectItem: Parser[LiteralObjectItem] = positioned {
    word ~ assignment ~ exprLiteral ^^ { case w ~ _ ~ l => LiteralObjectItem(w, l) }
  }

  /* ----- literal proto expression ----- */

  def exprLiteralProto: Parser[LiteralProto] = positioned {
    word ~ exprLiteralObject ^^ { case w ~ e => LiteralProto(w, e.items) }
  }

  /* ========== AST terminals ========== */

  def string: Parser[StringToken] = positioned {
    accept("string", { case token@StringToken(_) => token })
  }

  def number: Parser[NumberToken] = positioned {
    accept("number", { case token@NumberToken(_) => token })
  }

  def trueBool: Parser[TrueToken] = positioned {
    accept("trueBool", { case token@TrueToken() => token })
  }

  def falseBool: Parser[FalseToken] = positioned {
    accept("falseBool", { case token@FalseToken() => token })
  }

  def typeKeyword: Parser[TypeKeywordToken] = positioned {
    accept("typeKeyword", { case token@TypeKeywordToken() => token })
  }

  def confKeyword: Parser[ConfKeywordToken] = positioned {
    accept("confKeyword", { case token@ConfKeywordToken() => token })
  }

  def importKeyword: Parser[ImportKeywordToken] = positioned {
    accept("importKeyword", { case token@ImportKeywordToken() => token })
  }

  def word: Parser[WordToken] = positioned {
    accept("word", { case token@WordToken(_) => token })
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
