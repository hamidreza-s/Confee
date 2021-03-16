package com.spotify.confee

import com.spotify.confee.ConfeeIndexer.{ConfIndex, Index, ObjectInferredType}

import scala.util.{Failure, Success, Try}

object ConfeeConstructor {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      val index: List[Index] = ConfeeIndexer.indexStmts(stmts)
      Try(stmts.map {
        case confStmt @ ConfStmt(name, _, items) =>
          confStmt.copy(items = constructConfItems(items, name.word :: Nil, index))
        case otherwise => otherwise
      }) match {
        case Success(constructedStmts)    => Right(Grammar(constructedStmts))
        case Failure(ex: ConfeeCodeException) => Left(ConfeeEvaluatorError(ex.location, ex.msg))
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

  /* ----- evaluate config statement items ----- */

  def constructConfItems(
      confItems: ConfItems,
      parents: List[String],
      index: List[Index]
  ): ConfItems =
    ConfItems(confItems.items.map(item => constructConfItem(item, parents, index)))

  def constructConfItem(
      confItem: ConfItem,
      parents: List[String],
      index: List[Index]
  ): ConfItem =
    confItem match {
      case item @ ConfItem(name, itemVal: LiteralArray) =>
        item.copy(itemVal = constructLiteralArray(itemVal, name.value :: parents, index))
      case item @ ConfItem(name, itemVal: LiteralObject) =>
        item.copy(itemVal = constructLiteralObject(itemVal, name.value :: parents, index))
      case item @ ConfItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = constructLiteralProto(itemVal, name.value :: parents, index))
      case item => item
    }

  /* ----- construct literal expression ----- */

  def constructLiteralArray(
      literalArray: LiteralArray,
      parents: List[String],
      index: List[Index]
  ): LiteralArray =
    LiteralArray(literalArray.items.map(constructLiteralArrayItem(_, parents, index)))

  def constructLiteralArrayItem(
      arrayItem: LiteralExpr,
      parents: List[String],
      index: List[Index]
  ): LiteralExpr = arrayItem match {
    case literalArray: LiteralArray   => constructLiteralArray(literalArray, parents, index)
    case literalObject: LiteralObject => constructLiteralObject(literalObject, parents, index)
    case literalProto: LiteralProto   => constructLiteralProto(literalProto, parents, index)
    case item                         => item
  }

  /* ----- construct object expression ----- */

  def constructLiteralObject(
      literalObject: LiteralObject,
      parents: List[String],
      index: List[Index]
  ): LiteralObject =
    literalObject.copy(items = constructLiteralObjectItems(literalObject.items, parents, index))

  def constructLiteralObjectItems(
      literalObjectItems: LiteralObjectItems,
      parents: List[String],
      index: List[Index]
  ): LiteralObjectItems =
    LiteralObjectItems(
      literalObjectItems.items.map(item => constructLiteralObjectItem(item, parents, index))
    )

  def constructLiteralObjectItem(
      objectItem: LiteralObjectItem,
      parents: List[String],
      index: List[Index]
  ): LiteralObjectItem =
    objectItem match {
      case item @ LiteralObjectItem(name, itemVal: LiteralArray) =>
        item.copy(itemVal = constructLiteralArray(itemVal, name.value :: parents, index))
      case item @ LiteralObjectItem(name, itemVal: LiteralObject) =>
        item.copy(itemVal = constructLiteralObject(itemVal, name.value :: parents, index))
      case item @ LiteralObjectItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = constructLiteralProto(itemVal, name.value :: parents, index))
      case item => item
    }

  /* ----- construct proto expression ----- */

  def constructLiteralProto(
      literalProto: LiteralProto,
      parents: List[String],
      index: List[Index]
  ): LiteralObject = {
    // Lookup the object which is referenced as prototype
    val templateProto =
      ConfeeIndexer.exprIndexExpansion[LiteralObject](
        literalProto.name.value,
        ObjectInferredType,
        literalProto.pos,
        parents,
        index
      )

    // Make a map of items in the prototype as template to be used in the final constructed object
    val templateProtoItems = templateProto.items.items.foldLeft(Map.empty[String, LiteralExpr]) {
      case (acc, LiteralObjectItem(LiteralObjectItemKey(name), itemVal: LiteralExpr)) =>
        acc + (name -> itemVal)
    }

    // Make a map of final constructed object by overriding template items
    val literalProtoItems = literalProto.items.items.foldLeft(templateProtoItems) {
      case (acc, LiteralObjectItem(LiteralObjectItemKey(name), itemVal: LiteralExpr)) =>
        acc + (name -> itemVal)
    }

    // Make a list of final items of constructed literal objects
    val constructedItems = literalProtoItems.map {
      case (name, itemVal) => LiteralObjectItem(LiteralObjectItemKey(name), itemVal)
    }

    // Replace proto reference by a constructed object
    val constructedObject = templateProto.copy(items = LiteralObjectItems(constructedItems.toList))

    // Call object constructor to construct referenced prototypes in its items if available
    constructLiteralObject(constructedObject, literalProto.name.value :: parents, index)
  }
}
