package com.spotify.confee

import com.spotify.confee.ConfeeIndexer._

import scala.util.{Failure, Success, Try}

object ConfeeBinder {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      val index: List[ConfIndex] = indexConfStmts(stmts)
      Try(stmts.map {
        case confStmt @ ConfStmt(name, _, items) =>
          confStmt.copy(items = bindConfItems(items, name.word :: Nil, index))
        case otherwise => otherwise
      }) match {
        case Success(boundStmts)          => Right(Grammar(boundStmts))
        case Failure(ex: ConfeeException) => Left(ConfeeBinderError(ex.location, ex.msg))
        case Failure(ex)                  => Left(ConfeeUnknownError(ex))
      }

    case otherwise =>
      Left(
        ConfeeBinderError(
          Location(otherwise.pos.line, otherwise.pos.column),
          "AST in binder step does not contain valid grammar structure"
        )
      )
  }

  /* ----- bind config statement items ----- */

  def bindConfItems(
      confItems: ConfItems,
      parents: List[String],
      index: List[ConfIndex]
  ): ConfItems =
    ConfItems(confItems.items.map(item => bindConfItem(item, parents, index)))

  def bindConfItem(
      confItem: ConfItem,
      parents: List[String],
      index: List[ConfIndex]
  ): ConfItem =
    confItem match {
      case item @ ConfItem(_, itemVal: LiteralWord) =>
        item.copy(itemVal = bindLiteralWord(itemVal, parents, index))
      case item @ ConfItem(_, itemVal: LiteralBool) =>
        item.copy(itemVal = bindLiteralBool(itemVal, parents, index))
      case item @ ConfItem(_, itemVal: LiteralString) =>
        item.copy(itemVal = bindLiteralString(itemVal, parents, index))
      case item @ ConfItem(_, itemVal: LiteralNumber) =>
        item.copy(itemVal = bindLiteralNumber(itemVal, parents, index))
      case item @ ConfItem(_, itemVal: LiteralArray) =>
        item.copy(itemVal = bindLiteralArray(itemVal, parents, index))
      case item @ ConfItem(name, itemVal: LiteralObject) =>
        item.copy(itemVal = bindLiteralObject(itemVal, name.value :: parents, index))
      case item @ ConfItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = bindLiteralProto(itemVal, name.value :: parents, index))
    }

  /* ----- literal word expression ----- */

  def bindLiteralWord(
      word: LiteralWord,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralExpr = {
    ConfeeIndexer.indexLookup[LiteralExpr](word.value.word, NoInferredType, word.pos, parents, index)
  }

  /* ----- literal bool expression ----- */

  def bindLiteralBool(
      literalBool: LiteralBool,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralBool =
    literalBool match {
      case factor: LiteralBoolFactor => factor
      case word: LiteralBoolWord     => bindLiteralBoolWord(word, parents, index)
      case unit: LiteralBoolUnit     => bindLiteralBoolUnit(unit, parents, index)
      case group: LiteralBoolGroup   => bindLiteralBoolGroup(group, parents, index)
    }

  def bindLiteralBoolWord(
      word: LiteralBoolWord,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralBool =
    ConfeeIndexer.indexLookup[LiteralBool](word.value.word, BoolInferredType, word.pos, parents, index)

  @scala.annotation.tailrec
  def bindLiteralBoolUnit(
      unit: LiteralBoolUnit,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralBool = unit match {
    case same @ LiteralBoolUnit(_, _: LiteralBoolFactor) => same
    case LiteralBoolUnit(operator, unit) =>
      bindLiteralBoolUnit(
        LiteralBoolUnit(
          operator = operator,
          unit = bindLiteralBool(unit, parents, index)
        ),
        parents,
        index
      )
  }

  @scala.annotation.tailrec
  def bindLiteralBoolGroup(
      group: LiteralBoolGroup,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralBool =
    group match {
      case same @ LiteralBoolGroup(_, _: LiteralBoolFactor, _: LiteralBoolFactor) => same
      case LiteralBoolGroup(operator, left, right) =>
        bindLiteralBoolGroup(
          LiteralBoolGroup(
            operator = operator,
            left = bindLiteralBool(left, parents, index),
            right = bindLiteralBool(right, parents, index)
          ),
          parents,
          index
        )
    }

  /* ----- literal string expression ----- */

  def bindLiteralString(
      literalString: LiteralString,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralString =
    literalString match {
      case factor: LiteralStringFactor => factor
      case word: LiteralStringWord     => bindLiteralStringWord(word, parents, index)
      case group: LiteralStringGroup   => bindLiteralStringGroup(group, parents, index)
    }

  def bindLiteralStringWord(
      word: LiteralStringWord,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralString =
    ConfeeIndexer.indexLookup[LiteralString](word.value.word, StringInferredType, word.pos, parents, index)

  @scala.annotation.tailrec
  def bindLiteralStringGroup(
      group: LiteralStringGroup,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralString =
    group match {
      case same @ LiteralStringGroup(_, _: LiteralStringFactor, _: LiteralStringFactor) => same
      case LiteralStringGroup(operator, left, right) =>
        bindLiteralStringGroup(
          LiteralStringGroup(
            operator = operator,
            left = bindLiteralString(left, parents, index),
            right = bindLiteralString(right, parents, index)
          ),
          parents,
          index
        )
    }

  /* ----- literal number expression ----- */

  def bindLiteralNumber(
      literalNumber: LiteralNumber,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralNumber =
    literalNumber match {
      case factor: LiteralNumberFactor => factor
      case word: LiteralNumberWord     => bindLiteralNumberWord(word, parents, index)
      case group: LiteralNumberGroup   => bindLiteralNumberGroup(group, parents, index)
    }

  def bindLiteralNumberWord(
      word: LiteralNumberWord,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralNumber =
    ConfeeIndexer.indexLookup[LiteralNumber](word.value.word, NumberInferredType, word.pos, parents, index)

  @scala.annotation.tailrec
  def bindLiteralNumberGroup(
      group: LiteralNumberGroup,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralNumber =
    group match {
      case same @ LiteralNumberGroup(_, _: LiteralNumberFactor, _: LiteralNumberFactor) => same
      case LiteralNumberGroup(operator, left, right) =>
        bindLiteralNumberGroup(
          LiteralNumberGroup(
            operator = operator,
            left = bindLiteralNumber(left, parents, index),
            right = bindLiteralNumber(right, parents, index)
          ),
          parents,
          index
        )
    }

  /* ----- literal array expression ----- */

  def bindLiteralArray(
      literalArray: LiteralArray,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralArray =
    LiteralArray(literalArray.items.map(item => bindArrayItem(item, parents, index)))

  def bindArrayItem(
      arrayItem: LiteralExpr,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralExpr = arrayItem match {
    case literalWord: LiteralWord     => bindLiteralWord(literalWord, parents, index)
    case literalBool: LiteralBool     => bindLiteralBool(literalBool, parents, index)
    case literalString: LiteralString => bindLiteralString(literalString, parents, index)
    case literalNumber: LiteralNumber => bindLiteralNumber(literalNumber, parents, index)
    case literalArray: LiteralArray   => bindLiteralArray(literalArray, parents, index)
    case literalObject: LiteralObject => bindLiteralObject(literalObject, parents, index)
    case literalProto: LiteralProto   => bindLiteralProto(literalProto, parents, index)
  }

  /* ----- literal object expression ----- */

  def bindLiteralObject(
      literalObject: LiteralObject,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralObject =
    literalObject.copy(items = bindLiteralObjectItems(literalObject.items, parents, index))

  def bindLiteralObjectItems(
      literalObjectItems: LiteralObjectItems,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralObjectItems =
    LiteralObjectItems(
      literalObjectItems.items.map(item => bindLiteralObjectItem(item, parents, index))
    )

  def bindLiteralObjectItem(
      objectItem: LiteralObjectItem,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralObjectItem =
    objectItem match {
      case item @ LiteralObjectItem(_, itemVal: LiteralWord) =>
        item.copy(itemVal = bindLiteralWord(itemVal, parents, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralBool) =>
        item.copy(itemVal = bindLiteralBool(itemVal, parents, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralString) =>
        item.copy(itemVal = bindLiteralString(itemVal, parents, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralNumber) =>
        item.copy(itemVal = bindLiteralNumber(itemVal, parents, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralArray) =>
        item.copy(itemVal = bindLiteralArray(itemVal, parents, index))
      case item @ LiteralObjectItem(name, itemVal: LiteralObject) =>
        item.copy(itemVal = bindLiteralObject(itemVal, name.value :: parents, index))
      case item @ LiteralObjectItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = bindLiteralProto(itemVal, name.value :: parents, index))
    }

  /* ----- literal proto expression ----- */

  def bindLiteralProto(
      literalProto: LiteralProto,
      parents: List[String],
      index: List[ConfIndex]
  ): LiteralProto =
    literalProto.copy(items = bindLiteralObjectItems(literalProto.items, parents, index))

}
