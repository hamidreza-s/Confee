package com.spotify.confee

import com.spotify.confee.ConfeeHelper.hasReference

import scala.util.parsing.input.Position
import scala.util.{Failure, Success, Try}

object ConfeeConstructor {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      val index: List[IndexRow] = indexObjects(stmts)
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

  case class IndexRow(
      name: WordToken,
      parents: List[WordToken] = List.empty[WordToken],
      expr: LiteralObject,
      hasReference: Boolean
  )

  def indexObjects(stmts: List[Stmt]): List[IndexRow] = stmts.flatMap {
    case confStmt: ConfStmt => indexObjects(confStmt)
    case _                  => None
  }

  def indexObjects(confStmt: ConfStmt): List[IndexRow] = confStmt match {
    case ConfStmt(parent, _, items) =>
      items.items
        .foldLeft(List.empty[IndexRow]) {
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
      index: List[IndexRow] = List.empty[IndexRow]
  ): List[IndexRow] = expr match {
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
  ): List[IndexRow] =
    objectItems.items.flatMap { item =>
      indexObjects(name = item.name, expr = item.itemVal, parents = name :: parents)
    }

  // TODO: move it to a helper
  private def indexLookup(
      name: WordToken,
      pos: Position,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralObject =
    index
      .foldLeft(List.empty[(Int, Int, IndexRow)]) {
        case (acc, indexRow) =>
          val equalityHighPriority     = 0
          val equalityLowPriority      = 1
          val proximityPriority        = parents.size - indexRow.parents.size
          val hasSameName              = indexRow.name.word.equals(name.word)
          val hasSameExactParents      = indexRow.parents.equals(parents)
          val hasSameUpperLevelParents = indexRow.parents.forall(parents.contains)

          (hasSameName, hasSameExactParents, hasSameUpperLevelParents) match {
            case (true, true, _) => (equalityHighPriority, proximityPriority, indexRow) :: acc
            case (true, _, true) => (equalityLowPriority, proximityPriority, indexRow) :: acc
            case _               => acc
          }
      }
      .sortBy {
        case (parentalEqualityPriority, parentalProximityPriority, _) =>
          (parentalEqualityPriority, parentalProximityPriority)
      }
      .map {
        case (_, _, indexRow) => indexRow
      }
      .headOption match {
      case Some(indexRow) =>
        if (indexRow.hasReference) {
          throw ConfeeException(
            Location(pos.line, pos.column),
            s"Reference error: '${name.word}' has a circular reference"
          )
        } else {
          indexRow.expr
        }
      case None =>
        throw ConfeeException(
          Location(pos.line, pos.column),
          s"Reference error: '${name.word}' is not defined"
        )
    }

  /* ----- evaluate config statement items ----- */

  def constructConfItems(
      confItems: ConfItems,
      parents: List[WordToken],
      index: List[IndexRow]
  ): ConfItems =
    ConfItems(confItems.items.map(item => constructConfItem(item, parents, index)))

  def constructConfItem(
      confItem: ConfItem,
      parents: List[WordToken],
      index: List[IndexRow]
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
      index: List[IndexRow]
  ): LiteralObject =
    literalObject.copy(items = constructLiteralObjectItems(literalObject.items, parents, index))

  def constructLiteralObjectItems(
      literalObjectItems: LiteralObjectItems,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralObjectItems =
    LiteralObjectItems(
      literalObjectItems.items.map(item => constructLiteralObjectItem(item, parents, index))
    )

  def constructLiteralObjectItem(
      objectItem: LiteralObjectItem,
      parents: List[WordToken],
      index: List[IndexRow]
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
      index: List[IndexRow]
  ): LiteralObject = {
    // Lookup the object which is referenced as prototype
    val templateProto = indexLookup(literalProto.proto, literalProto.pos, parents, index)

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
