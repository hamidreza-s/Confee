package com.spotify.confee

import scala.util.parsing.input.Positional

/**
  * AST Structure
  *
  * - Grammar is a list of Statements
  * - Statement can contain another Statement, an Expression or a Node
  * - Expression can contain another Expression or a Node
  * - Node can contain token which generated in the lexing phase
  *
  *           Grammar
  *           /     \
  *         Stmt  Stmt
  *         /  \     \
  *      Expr  Stmt  Node
  *      /  \
  *    Expr Node
  */

sealed trait ConfeeAST extends Positional

sealed trait Stmt extends ConfeeAST

sealed trait Expr extends ConfeeAST

sealed trait Node extends ConfeeAST

/* grammar */

case class Grammar(stmts: List[Stmt]) extends ConfeeAST

/* type statement */

case class TypeStmt(name: NameToken, items: List[TypeItem]) extends Stmt

case class TypeItem(name: WordToken, itemType: TypeDef) extends Node

case class TypeDef(name: Either[NameToken, WordToken], isList: Boolean) extends Node

/* fact statement */

case class FactStmt(name: WordToken, factType: TypeDef, items: List[FactItem]) extends Stmt

case class FactItem(name: WordToken, itemType: TypeDef) extends Node

/* debugging statement */

case class DebuggingStmt(name: String, items: List[Any]) extends ConfeeAST
