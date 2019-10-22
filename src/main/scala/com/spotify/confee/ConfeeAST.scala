package com.spotify.confee

import scala.util.parsing.input.Positional

sealed trait ConfeeAST extends Positional

case class Stmts(stmts: List[ConfeeAST]) extends ConfeeAST

case class TypeStmt(name: NameToken, items: List[TypeItem]) extends ConfeeAST
case class TypeItem(name: WordToken, itemType: TypeDef) extends ConfeeAST
case class TypeDef(name: Either[NameToken, WordToken], isList: Boolean) extends ConfeeAST

case class FactStmt(name: WordToken, factType: TypeDef, items: List[FactItem]) extends ConfeeAST
case class FactItem(name: WordToken, itemType: TypeDef) extends ConfeeAST

case class DebuggingStmt(name: String, items: List[Any]) extends ConfeeAST

