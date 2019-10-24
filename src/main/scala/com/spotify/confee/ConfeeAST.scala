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
  * - Expression can not be defined in top-level (literal, condition, case, arithmetic operation)
  * - Node is an abstraction which contains a value or points to it
  * - Node can be a Token which generated in the lexing phase
  * - Node can also point to a Statement, an Expression, or another Node
  *
  *           Grammar
  *           /     \
  *         Stmt  Stmt
  *         /  \     \
  *      Expr  Stmt  Node -> Stmt | Expr | Node
  *      /  \
  *    Expr Node
  *          \\
  *         Token
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

case class FactItem(name: WordToken, itemVal: Literal) extends Node

case class FactItems(items: List[FactItem]) extends Node

/* literal */

sealed trait Literal extends Expr

case class StringLiteral(value: StringToken) extends Literal

case class NumberLiteral(value: NumberToken) extends Literal

case class ListLiteral[T](value: List[T]) extends Literal

/* debugging statement */

case class DebuggingStmt(name: String, items: List[Any]) extends ConfeeAST
