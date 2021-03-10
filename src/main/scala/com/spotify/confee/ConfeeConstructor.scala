package com.spotify.confee

import com.spotify.confee.ConfeeHelper.hasReference
import com.spotify.confee.ConfeeIndexer.IndexRow

import scala.util.{Failure, Success, Try}

object ConfeeConstructor {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      val index: List[IndexRow[LiteralObject]] = indexObjects(stmts)
      Try(stmts.map {
        case confStmt @ ConfStmt(name, _, items) =>
          confStmt.copy(items = constructConfItems(items, name :: Nil, index))
        case otherwise => otherwise
      }) match {
        case Success(constructedStmts)    => Right(Grammar(constructedStmts))
        case Failure(ex: ConfeeException) => Left(ConfeeEvaluatorError(ex.location, ex.msg))
        case Failure(ex)                  => Left(ConfeeUnknownError(ex))
      }
    case otherwise =>
      Left(
        ConfeeConstructorError(
          Location(otherwise.pos.line, otherwise.pos.column),
          "AST in evaluation step does not contain valid grammar structure"
        )
      )
  }

  /* ----- index objects ----- */

  def indexObjects(stmts: List[Stmt]): List[IndexRow[LiteralObject]] = stmts.flatMap {
    case confStmt: ConfStmt => indexObjects(confStmt)
    case _                  => None
  }

  def indexObjects(confStmt: ConfStmt): List[IndexRow[LiteralObject]] = confStmt match {
    case ConfStmt(parent, _, items) =>
      items.items
        .foldLeft(List.empty[IndexRow[LiteralObject]]) {
          case (acc, ConfItem(name: WordToken, itemVal: LiteralObject)) =>
            indexObjects(
              name = name,
              expr = itemVal,
              parents = List(parent),
              index = acc
            )
          case (acc, _) => acc
        }
  }

  def indexObjects(
      name: WordToken,
      expr: Expr,
      parents: List[WordToken] = List.empty[WordToken],
      index: List[IndexRow[LiteralObject]] = List.empty[IndexRow[LiteralObject]]
  ): List[IndexRow[LiteralObject]] = expr match {
    case literalObject: LiteralObject =>
      index ::: IndexRow(name, parents, literalObject, hasReference(literalObject)) :: indexObjectItems(
        name,
        literalObject.items,
        parents
      )
    case _ =>
      index
  }

  def indexObjectItems(
      name: WordToken,
      objectItems: LiteralObjectItems,
      parents: List[WordToken]
  ): List[IndexRow[LiteralObject]] =
    objectItems.items.flatMap { item =>
      indexObjects(name = item.name, expr = item.itemVal, parents = name :: parents)
    }

  /* ----- evaluate config statement items ----- */

  def constructConfItems(
      confItems: ConfItems,
      parents: List[WordToken],
      index: List[IndexRow[LiteralObject]]
  ): ConfItems =
    ConfItems(confItems.items.map(item => constructConfItem(item, parents, index)))

  def constructConfItem(
      confItem: ConfItem,
      parents: List[WordToken],
      index: List[IndexRow[LiteralObject]]
  ): ConfItem =
    confItem match {
      case item @ ConfItem(name, itemVal: LiteralObject) =>
        item.copy(itemVal = constructLiteralObject(itemVal, name :: parents, index))
      case item @ ConfItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = constructLiteralProto(itemVal, name :: parents, index))
      case item => item
    }

  /* ----- construct object expression ----- */

  def constructLiteralObject(
      literalObject: LiteralObject,
      parents: List[WordToken],
      index: List[IndexRow[LiteralObject]]
  ): LiteralObject =
    literalObject.copy(items = constructLiteralObjectItems(literalObject.items, parents, index))

  def constructLiteralObjectItems(
      literalObjectItems: LiteralObjectItems,
      parents: List[WordToken],
      index: List[IndexRow[LiteralObject]]
  ): LiteralObjectItems =
    LiteralObjectItems(
      literalObjectItems.items.map(item => constructLiteralObjectItem(item, parents, index))
    )

  def constructLiteralObjectItem(
      objectItem: LiteralObjectItem,
      parents: List[WordToken],
      index: List[IndexRow[LiteralObject]]
  ): LiteralObjectItem =
    objectItem match {
      case item @ LiteralObjectItem(name, itemVal: LiteralObject) =>
        item.copy(itemVal = constructLiteralObject(itemVal, name :: parents, index))
      case item @ LiteralObjectItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = constructLiteralProto(itemVal, name :: parents, index))
      case item => item
    }

  /* ----- construct proto expression ----- */

  def constructLiteralProto(
      literalProto: LiteralProto,
      parents: List[WordToken],
      index: List[IndexRow[LiteralObject]]
  ): LiteralObject = {
    // Lookup the object which is referenced as prototype
    val templateProto =
      ConfeeIndexer.indexLookup(literalProto.proto, literalProto.pos, parents, index)

    // Make a map of items in the prototype as template to be used in the final constructed object
    val templateProtoItems = templateProto.items.items.foldLeft(Map.empty[WordToken, LiteralExpr]) {
      case (acc, LiteralObjectItem(name: WordToken, itemVal: LiteralExpr)) =>
        acc + (name -> itemVal)
    }

    // Make a map of final constructed object by overriding template items
    val literalProtoItems = literalProto.items.items.foldLeft(templateProtoItems) {
      case (acc, LiteralObjectItem(name: WordToken, itemVal: LiteralExpr)) =>
        acc + (name -> itemVal)
    }

    // Make a list of final items of constructed literal objects
    val constructedItems = literalProtoItems.map {
      case (name, itemVal) => LiteralObjectItem(name, itemVal)
    }

    // Replace proto reference by a constructed object
    val constructedObject = templateProto.copy(items = LiteralObjectItems(constructedItems.toList))

    // Call object constructor to construct referenced prototypes in its items if available
    constructLiteralObject(constructedObject, literalProto.proto :: parents, index)
  }
}
