package com.spotify.confee

import scala.util.{Failure, Success, Try}

object ConfeeBinder {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      val index = indexStmts(stmts) // TODO: use index to replace references with words
      Try(stmts.map {
        case confStmt @ ConfStmt(_, _, items) => confStmt.copy(items = bindConfItems(items))
        case otherwise                        => otherwise
      }) match {
        case Success(boundStmts)          => Right(Grammar(boundStmts))
        case Failure(ex: ConfeeException) => Left(ConfeeBinderError(ex.location, ex.msg))
        case Failure(ex)                  => Left(ConfeeUnknownError(ex))
      }

    case otherwise =>
      Left(
        ConfeeBinderError(
          Location(otherwise.pos.line, otherwise.pos.column),
          "AST in binder input does not contain valid grammar structure"
        )
      )
  }

  /* ----- index config / object / proto items ----- */

  case class IndexRow(
      name: WordToken,
      parent: List[WordToken] = List.empty[WordToken],
      expr: Expr,
      hasReference: Boolean
  )

  def indexStmts(stmts: List[Stmt]): List[IndexRow] = stmts.flatMap {
    case confStmt: ConfStmt => indexConfStmt(confStmt)
    case _                  => None
  }

  def indexConfStmt(confStmt: ConfStmt): List[IndexRow] = confStmt match {
    case ConfStmt(name, _, items) =>
      items.items
        .foldLeft(List.empty[IndexRow]) {
          case (acc, item) =>
            indexConfItem(
              name = item.name,
              parent = List(name),
              expr = item.itemVal,
              index = acc
            )
        }
  }

  def indexConfItem(
      name: WordToken,
      expr: Expr,
      parent: List[WordToken] = List.empty[WordToken],
      index: List[IndexRow] = List.empty[IndexRow]
  ): List[IndexRow] = expr match {
    case LiteralObject(items: LiteralObjectItems) =>
      index ::: IndexRow(name, parent, expr, hasReference(expr)) :: indexObjectItems(
        items,
        name,
        expr,
        parent
      )
    case LiteralProto(_, items: LiteralObjectItems) =>
      index ::: IndexRow(name, parent, expr, hasReference(expr)) :: indexObjectItems(
        items,
        name,
        expr,
        parent
      )
    case _ =>
      index ::: IndexRow(name, parent, expr, hasReference(expr)) :: Nil
  }

  def indexObjectItems(
      objectItems: LiteralObjectItems,
      name: WordToken,
      expr: Expr,
      parent: List[WordToken]
  ): List[IndexRow] =
    objectItems.items.flatMap { item =>
      indexConfItem(name = item.name, expr = item.itemVal, parent = name :: parent)
    }

  def hasReference(expr: Expr): Boolean = expr match {
    // literal bool expressions
    case _: LiteralBoolFactor => false
    case _: LiteralBoolWord   => true
    case e: LiteralBoolUnit   => hasReference(e.unit)
    case e: LiteralBoolGroup  => hasReference(e.left) || hasReference(e.right)
    // literal string expressions
    case _: LiteralStringFactor => false
    case _: LiteralStringWord   => true
    case e: LiteralStringGroup  => hasReference(e.left) || hasReference(e.right)
    // literal number expressions
    case _: LiteralNumberFactor => false
    case _: LiteralNumberWord   => true
    case e: LiteralNumberGroup  => hasReference(e.left) || hasReference(e.right)
    // literal Array expressions
    case e: LiteralArray => e.items.exists(hasReference)
    // literal object expressions
    case e: LiteralObject => e.items.items.exists(i => hasReference(i.itemVal))
    // literal proto expressions
    case e: LiteralProto => e.items.items.exists(i => hasReference(i.itemVal))
  }

  /* ----- bind config statement items ----- */

  def bindConfItems(confItems: ConfItems): ConfItems =
    ConfItems(confItems.items.map(bindConfItem))

  def bindConfItem(confItem: ConfItem): ConfItem = confItem match {
    case item @ ConfItem(_, itemVal: LiteralBool) =>
      item.copy(itemVal = bindLiteralBool(itemVal))
    case item @ ConfItem(_, itemVal: LiteralString) =>
      item.copy(itemVal = bindLiteralString(itemVal))
    case item @ ConfItem(_, itemVal: LiteralNumber) =>
      item.copy(itemVal = bindLiteralNumber(itemVal))
    case item @ ConfItem(_, itemVal: LiteralArray) =>
      item.copy(itemVal = bindLiteralArray(itemVal))
    case item @ ConfItem(_, itemVal: LiteralObject) =>
      item.copy(itemVal = bindLiteralObject(itemVal))
    case item @ ConfItem(_, itemVal: LiteralProto) =>
      item.copy(itemVal = bindLiteralProto(itemVal))
  }

  /* ----- tbd ----- */

  def bindLiteralBool(literalBool: LiteralBool): LiteralBool = literalBool

  def bindLiteralString(literalString: LiteralString): LiteralString = literalString

  def bindLiteralNumber(literalNumber: LiteralNumber): LiteralNumber = literalNumber

  def bindLiteralArray(literalArray: LiteralArray): LiteralArray = literalArray

  def bindLiteralObject(literalObject: LiteralObject): LiteralObject = literalObject

  def bindLiteralProto(literalProto: LiteralProto): LiteralProto = literalProto

}
