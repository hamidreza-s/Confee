package com.spotify.confee

import scala.util.parsing.input.Positional

/**
  * AST Structure
  *
  * - Grammar is the root, Statement, Expression and Node are non-terminal, and Node is terminal
  * - Grammar is a list of Statements
  * - Statement can contain another Statement, an Expression or a Node
  * - Statement has scope and can be defined in top-level (type, conf, import, export)
  * - Expression can contain another Expression or a Node
  * - Expression can not be defined in top-level (literal, lambda, condition, case)
  * - Literal Expression can be bool, string, number, array, object or other Expressions
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
  *      Literal | Lambda | Condition | Case <= Expr Node
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

/* conf statement */

case class ConfStmt(name: WordToken, confType: TypeDef, items: ConfItems) extends Stmt

case class ConfItem(name: WordToken, itemVal: Expr) extends Node

case class ConfItems(items: List[ConfItem]) extends Node

/* literal expression */

sealed trait LiteralExpr extends Expr

/* literal boolean expression */

sealed trait LiteralBool extends LiteralExpr

case class LiteralBoolTrue() extends LiteralBool

case class LiteralBoolFalse() extends LiteralBool

/* literal string expression */

sealed trait LiteralString extends LiteralExpr

sealed trait LiteralStringOperator extends Node

case class LiteralStringFactor(value: StringToken) extends LiteralString

case class LiteralStringWord(value: WordToken) extends LiteralString

case class LiteralStringGroup(operator: LiteralStringOperator,
                              left: LiteralString,
                              right: LiteralString) extends LiteralString

case class LiteralStringOperatorConcat() extends LiteralStringOperator

case class LiteralStringOperatorRemove() extends LiteralStringOperator

/* literal number expression */

sealed trait LiteralNumber extends LiteralExpr

sealed trait LiteralNumberOperator extends Node

case class LiteralNumberFactor(value: NumberToken) extends LiteralNumber

case class LiteralNumberWord(value: WordToken) extends LiteralNumber

case class LiteralNumberGroup(operator: LiteralNumberOperator,
                              left: LiteralNumber,
                              right: LiteralNumber) extends LiteralNumber

case class LiteralNumberOperatorAdd() extends LiteralNumberOperator

case class LiteralNumberOperatorSub() extends LiteralNumberOperator

case class LiteralNumberOperatorDiv() extends LiteralNumberOperator

case class LiteralNumberOperatorMul() extends LiteralNumberOperator

case class LiteralNumberOperatorMod() extends LiteralNumberOperator

/* literal array expression */

case class LiteralArray(value: List[LiteralExpr]) extends LiteralExpr

/* literal object expression */

case class LiteralObject(items: LiteralObjectItems) extends LiteralExpr

case class LiteralObjectItem(name: WordToken, itemVal: LiteralExpr) extends Node

case class LiteralObjectItems(items: List[LiteralObjectItem]) extends Node

/* debugging statement */

case class DebuggingStmt(name: String, items: List[Any]) extends ConfeeAST
