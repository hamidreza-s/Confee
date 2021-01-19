package com.spotify.confee

import scala.util.parsing.input.Position
import scala.util.{Failure, Success, Try}

object ConfeeBinder {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      val index: List[IndexRow] = indexStmts(stmts)
      Try(stmts.map {
        case confStmt @ ConfStmt(_, _, items) => confStmt.copy(items = bindConfItems(items, index))
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
          "AST in binder step does not contain valid grammar structure"
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
    // literal word expressions
    case _: LiteralWord => true
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

  private def indexLookup(word: WordToken, pos: Position, index: List[IndexRow]): Expr = {
    index
      .find {
        case IndexRow(name, _, _, _) =>
          name.word.equals(word.word)
      } match {
      case Some(indexRow) =>
        if (indexRow.hasReference) {
          throw ConfeeException(
            Location(pos.line, pos.column),
            s"Reference error: '${word.word}' has a circular reference"
          )
        } else {
          indexRow.expr
        }
      case None =>
        throw ConfeeException(
          Location(pos.line, pos.column),
          s"Reference error: '${word.word}' is not defined"
        )
    }
  }

  /* ----- bind config statement items ----- */

  def bindConfItems(confItems: ConfItems, index: List[IndexRow]): ConfItems =
    ConfItems(confItems.items.map(item => bindConfItem(item, index)))

  def bindConfItem(confItem: ConfItem, index: List[IndexRow]): ConfItem = confItem match {
    case item @ ConfItem(_, itemVal: LiteralWord) =>
      item.copy(itemVal = bindLiteralWord(itemVal, index))
    case item @ ConfItem(_, itemVal: LiteralBool) =>
      item.copy(itemVal = bindLiteralBool(itemVal, index))
    case item @ ConfItem(_, itemVal: LiteralString) =>
      item.copy(itemVal = bindLiteralString(itemVal, index))
    case item @ ConfItem(_, itemVal: LiteralNumber) =>
      item.copy(itemVal = bindLiteralNumber(itemVal, index))
    case item @ ConfItem(_, itemVal: LiteralArray) =>
      item.copy(itemVal = bindLiteralArray(itemVal, index))
    case item @ ConfItem(_, itemVal: LiteralObject) =>
      item.copy(itemVal = bindLiteralObject(itemVal, index))
    case item @ ConfItem(_, itemVal: LiteralProto) =>
      item.copy(itemVal = bindLiteralProto(itemVal, index))
  }

  /* ----- literal word expression ----- */

  def bindLiteralWord(word: LiteralWord, index: List[IndexRow]): LiteralExpr =
    indexLookup(word.value, word.pos, index).asInstanceOf[LiteralExpr]

  /* ----- literal bool expression ----- */

  def bindLiteralBool(literalBool: LiteralBool, index: List[IndexRow]): LiteralBool =
    literalBool match {
      case factor: LiteralBoolFactor => factor
      case word: LiteralBoolWord     => bindLiteralBoolWord(word, index)
      case unit: LiteralBoolUnit     => bindLiteralBoolUnit(unit, index)
      case group: LiteralBoolGroup   => bindLiteralBoolGroup(group, index)
    }

  def bindLiteralBoolWord(word: LiteralBoolWord, index: List[IndexRow]): LiteralBool =
    indexLookup(word.value, word.pos, index).asInstanceOf[LiteralBool]

  @scala.annotation.tailrec
  def bindLiteralBoolUnit(unit: LiteralBoolUnit, index: List[IndexRow]): LiteralBool = unit match {
    case same @ LiteralBoolUnit(_, _: LiteralBoolFactor) => same
    case LiteralBoolUnit(operator, unit) =>
      bindLiteralBoolUnit(
        LiteralBoolUnit(
          operator = operator,
          unit = bindLiteralBool(unit, index)
        ),
        index
      )
  }

  @scala.annotation.tailrec
  def bindLiteralBoolGroup(group: LiteralBoolGroup, index: List[IndexRow]): LiteralBool =
    group match {
      case same @ LiteralBoolGroup(_, _: LiteralBoolFactor, _: LiteralBoolFactor) => same
      case LiteralBoolGroup(operator, left, right) =>
        bindLiteralBoolGroup(
          LiteralBoolGroup(
            operator = operator,
            left = bindLiteralBool(left, index),
            right = bindLiteralBool(right, index)
          ),
          index
        )
    }

  /* ----- literal string expression ----- */

  def bindLiteralString(literalString: LiteralString, index: List[IndexRow]): LiteralString =
    literalString match {
      case factor: LiteralStringFactor => factor
      case word: LiteralStringWord     => bindLiteralStringWord(word, index)
      case group: LiteralStringGroup   => bindLiteralStringGroup(group, index)
    }

  def bindLiteralStringWord(word: LiteralStringWord, index: List[IndexRow]): LiteralString =
    indexLookup(word.value, word.pos, index).asInstanceOf[LiteralString]

  @scala.annotation.tailrec
  def bindLiteralStringGroup(group: LiteralStringGroup, index: List[IndexRow]): LiteralString =
    group match {
      case same @ LiteralStringGroup(_, _: LiteralStringFactor, _: LiteralStringFactor) => same
      case LiteralStringGroup(operator, left, right) =>
        bindLiteralStringGroup(
          LiteralStringGroup(
            operator = operator,
            left = bindLiteralString(left, index),
            right = bindLiteralString(right, index)
          ),
          index
        )
    }

  /* ----- literal number expression ----- */

  def bindLiteralNumber(literalNumber: LiteralNumber, index: List[IndexRow]): LiteralNumber =
    literalNumber match {
      case factor: LiteralNumberFactor => factor
      case word: LiteralNumberWord     => bindLiteralNumberWord(word, index)
      case group: LiteralNumberGroup   => bindLiteralNumberGroup(group, index)
    }

  def bindLiteralNumberWord(word: LiteralNumberWord, index: List[IndexRow]): LiteralNumber =
    indexLookup(word.value, word.pos, index).asInstanceOf[LiteralNumber]

  @scala.annotation.tailrec
  def bindLiteralNumberGroup(group: LiteralNumberGroup, index: List[IndexRow]): LiteralNumber =
    group match {
      case same @ LiteralNumberGroup(_, _: LiteralNumberFactor, _: LiteralNumberFactor) => same
      case LiteralNumberGroup(operator, left, right) =>
        bindLiteralNumberGroup(
          LiteralNumberGroup(
            operator = operator,
            left = bindLiteralNumber(left, index),
            right = bindLiteralNumber(right, index)
          ),
          index
        )
    }

  /* ----- literal array expression ----- */

  def bindLiteralArray(literalArray: LiteralArray, index: List[IndexRow]): LiteralArray =
    LiteralArray(literalArray.items.map(item => bindArrayItem(item, index)))

  def bindArrayItem(arrayItem: LiteralExpr, index: List[IndexRow]): LiteralExpr = arrayItem match {
    case literalWord: LiteralWord     => bindLiteralWord(literalWord, index)
    case literalBool: LiteralBool     => bindLiteralBool(literalBool, index)
    case literalString: LiteralString => bindLiteralString(literalString, index)
    case literalNumber: LiteralNumber => bindLiteralNumber(literalNumber, index)
    case literalArray: LiteralArray   => bindLiteralArray(literalArray, index)
    case literalObject: LiteralObject => bindLiteralObject(literalObject, index)
    case literalProto: LiteralProto   => bindLiteralProto(literalProto, index)
  }

  /* ----- literal object expression ----- */

  def bindLiteralObject(literalObject: LiteralObject, index: List[IndexRow]): LiteralObject =
    literalObject.copy(items = bindLiteralObjectItems(literalObject.items, index))

  def bindLiteralObjectItems(
      literalObjectItems: LiteralObjectItems,
      index: List[IndexRow]
  ): LiteralObjectItems =
    LiteralObjectItems(literalObjectItems.items.map(item => bindLiteralObjectItem(item, index)))

  def bindLiteralObjectItem(
      objectItem: LiteralObjectItem,
      index: List[IndexRow]
  ): LiteralObjectItem =
    objectItem match {
      case item @ LiteralObjectItem(_, itemVal: LiteralWord) =>
        item.copy(itemVal = bindLiteralWord(itemVal, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralBool) =>
        item.copy(itemVal = bindLiteralBool(itemVal, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralString) =>
        item.copy(itemVal = bindLiteralString(itemVal, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralNumber) =>
        item.copy(itemVal = bindLiteralNumber(itemVal, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralArray) =>
        item.copy(itemVal = bindLiteralArray(itemVal, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralObject) =>
        item.copy(itemVal = bindLiteralObject(itemVal, index))
      case item @ LiteralObjectItem(_, itemVal: LiteralProto) =>
        item.copy(itemVal = bindLiteralProto(itemVal, index))
    }

  /* ----- literal proto expression ----- */

  def bindLiteralProto(literalProto: LiteralProto, index: List[IndexRow]): LiteralProto =
    literalProto.copy(items = bindLiteralObjectItems(literalProto.items, index))

}
