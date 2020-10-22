package com.spotify.confee

import scala.util.parsing.input.Positional

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

/* import statement */

case class ImportStmt(path: StringToken) extends Stmt

/* literal expression */

sealed trait LiteralExpr extends Expr

/* literal boolean (bitwise) expression */

sealed trait LiteralBool extends LiteralExpr

sealed trait LiteralBoolOperator extends Node

case class LiteralBoolWord(value: WordToken) extends LiteralBool

case class LiteralBoolFactor(value: BoolToken) extends LiteralBool

case class LiteralBoolGroup(
    operator: LiteralBoolOperator,
    left: LiteralBool,
    right: LiteralBool
) extends LiteralBool

case class LiteralBoolOperatorNot() extends LiteralBoolOperator

case class LiteralBoolOperatorAnd() extends LiteralBoolOperator

case class LiteralBoolOperatorOr() extends LiteralBoolOperator

case class LiteralBoolOperatorXor() extends LiteralBoolOperator

/* literal string expression */

sealed trait LiteralString extends LiteralExpr

sealed trait LiteralStringOperator extends Node

case class LiteralStringFactor(value: StringToken) extends LiteralString

case class LiteralStringWord(value: WordToken) extends LiteralString

case class LiteralStringGroup(
    operator: LiteralStringOperator,
    left: LiteralString,
    right: LiteralString
) extends LiteralString

case class LiteralStringOperatorConcat() extends LiteralStringOperator

case class LiteralStringOperatorRemove() extends LiteralStringOperator

/* literal number expression */

sealed trait LiteralNumber extends LiteralExpr

sealed trait LiteralNumberOperator extends Node

case class LiteralNumberFactor(value: NumberToken) extends LiteralNumber

case class LiteralNumberWord(value: WordToken) extends LiteralNumber

case class LiteralNumberGroup(
    operator: LiteralNumberOperator,
    left: LiteralNumber,
    right: LiteralNumber
) extends LiteralNumber

case class LiteralNumberOperatorAdd() extends LiteralNumberOperator

case class LiteralNumberOperatorSub() extends LiteralNumberOperator

case class LiteralNumberOperatorDiv() extends LiteralNumberOperator

case class LiteralNumberOperatorMul() extends LiteralNumberOperator

case class LiteralNumberOperatorMod() extends LiteralNumberOperator

/* literal array expression */

case class LiteralArray(items: List[LiteralExpr]) extends LiteralExpr

/* literal object expression */

case class LiteralObject(items: LiteralObjectItems) extends LiteralExpr

case class LiteralObjectItem(name: WordToken, itemVal: LiteralExpr) extends Node

case class LiteralObjectItems(items: List[LiteralObjectItem]) extends Node

/* literal proto expression */

case class LiteralProto(proto: WordToken, items: LiteralObjectItems) extends LiteralExpr

/* debugging statement */

case class DebuggingStmt(name: String, items: List[Any]) extends ConfeeAST
