package com.spotify.confee

import scala.util.parsing.input.Positional

/**
  * AST Structure
  *
  * - Grammar is the root, Statement, Expression and Node are non-terminal, and Node is terminal
  * - Grammar is a list of Statements
  * - Statement can contain another Statement, an Expression or a Node
  * - Statement has scope and can be defined in top-level (type, fact, func, import, export)
  * - Expression can contain another Expression or a Node
  * - Expression can not be defined in top-level (literal, arithmetic, condition, case)
  * - Literal Expression can be string, number, list, tuple, lambda or other Expressions
  * - Node is an abstraction which contains a value or points to it
  * - Node can be a Token which generated in the lexing phase
  * - Node can also point to a Statement, an Expression, or another Node
  *
  *                                                    Grammar
  *                                                    /     \
  *                                                  Stmt  Stmt
  *                                                  /  \     \
  *                                               Expr  Stmt  Node -> Stmt | Expr | Node
  *                                               /  \
  *  Literal | Arithmetic | Condition | Case <= Expr Node
  *                                                   \\
  *                                                  Token
  */

sealed trait ConfeeAST extends Positional

sealed trait Stmt extends ConfeeAST

sealed trait Expr extends ConfeeAST

protected trait Node extends ConfeeAST

/* grammar */

case class Grammar(stmts: List[Stmt]) extends ConfeeAST

/* type statement */

case class TypeStmt(name: NameToken, items: TypeItems) extends Stmt

case class TypeItem(name: WordToken, itemType: TypeDef) extends Node

case class TypeItems(items: List[TypeItem]) extends Node

case class TypeDef(name: Either[NameToken, WordToken], isList: Boolean) extends Node

/* fact statement */

case class FactStmt(name: WordToken, factType: TypeDef, items: FactItems) extends Stmt

case class FactItem(name: WordToken, itemVal: Expr) extends Node

case class FactItems(items: List[FactItem]) extends Node

/* literal expression */

sealed trait LiteralExpr extends Expr

case class LiteralString(value: StringToken) extends LiteralExpr

case class LiteralNumber(value: NumberToken) extends LiteralExpr

case class LiteralList(value: List[LiteralExpr]) extends LiteralExpr

/* arithmetic expression */

sealed trait ArithExpr extends Expr

sealed trait ArithOperator extends Node

case class ArithFactorGroup(operator: ArithOperator, a: ArithExpr, b: ArithExpr) extends ArithExpr

case class ArithFactorNumber(value: NumberToken) extends ArithExpr

case class ArithFactorWord(value: WordToken) extends ArithExpr

case class ArithAddOperator() extends ArithOperator

case class ArithSubOperator() extends ArithOperator

case class ArithDivOperator() extends ArithOperator

case class ArithMulOperator() extends ArithOperator

case class ArithModOperator() extends ArithOperator

/* debugging statement */

case class DebuggingStmt(name: String, items: List[Any]) extends ConfeeAST
