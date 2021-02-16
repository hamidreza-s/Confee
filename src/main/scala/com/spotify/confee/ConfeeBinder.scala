package com.spotify.confee

import scala.util.parsing.input.Position
import scala.util.{Failure, Success, Try}

object ConfeeBinder {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      val index: List[IndexRow] = indexStmts(stmts)
      Try(stmts.map {
        case confStmt @ ConfStmt(name, _, items) =>
          confStmt.copy(items = bindConfItems(items, name :: Nil, index))
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

  /* ----- index config / object / proto items ----- */

  case class IndexRow(
      name: WordToken,
      parents: List[WordToken] = List.empty[WordToken],
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
              parents = List(name),
              expr = item.itemVal,
              index = acc
            )
        }
  }

  def indexConfItem(
      name: WordToken,
      expr: Expr,
      parents: List[WordToken] = List.empty[WordToken],
      index: List[IndexRow] = List.empty[IndexRow]
  ): List[IndexRow] = expr match {
    case LiteralObject(items: LiteralObjectItems) =>
      index ::: IndexRow(name, parents, expr, hasReference(expr)) :: indexObjectItems(
        items,
        name,
        expr,
        parents
      )
    case LiteralProto(_, items: LiteralObjectItems) =>
      index ::: IndexRow(name, parents, expr, hasReference(expr)) :: indexObjectItems(
        items,
        name,
        expr,
        parents
      )
    case _ =>
      index ::: IndexRow(name, parents, expr, hasReference(expr)) :: Nil
  }

  def indexObjectItems(
      objectItems: LiteralObjectItems,
      name: WordToken,
      expr: Expr,
      parent: List[WordToken]
  ): List[IndexRow] =
    objectItems.items.flatMap { item =>
      indexConfItem(name = item.name, expr = item.itemVal, parents = name :: parent)
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

  private def isSameWord(a: WordToken, b: WordToken): Boolean =
    a.word == b.word & a.pos.line == b.pos.line & a.pos.column == b.pos.column

  private def indexLookup(
      name: WordToken,
      pos: Position,
      parents: List[WordToken],
      index: List[IndexRow]
  ): Expr = {
    index
      .foldLeft(List.empty[IndexRow]) {
        case (acc, indexRow @ IndexRow(indexedName, indexedParents, _, _)) =>
          val hasSameName              = indexedName.word.equals(name.word)
          val hasSameExactParents      = indexedParents.eq(parents)
          val hasSameUpperLevelParents = indexedParents.forall(parents.contains)
          if (hasSameName & (hasSameExactParents | hasSameUpperLevelParents)) List(indexRow)
          else acc
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
  }

  /* ----- bind config statement items ----- */

  def bindConfItems(
      confItems: ConfItems,
      parents: List[WordToken],
      index: List[IndexRow]
  ): ConfItems =
    ConfItems(confItems.items.map(item => bindConfItem(item, parents, index)))

  def bindConfItem(confItem: ConfItem, parents: List[WordToken], index: List[IndexRow]): ConfItem =
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
        item.copy(itemVal = bindLiteralObject(itemVal, name :: parents, index))
      case item @ ConfItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = bindLiteralProto(itemVal, name :: parents, index))
    }

  /* ----- literal word expression ----- */

  def bindLiteralWord(
      word: LiteralWord,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralExpr =
    indexLookup(word.value, word.pos, parents, index).asInstanceOf[LiteralExpr]

  /* ----- literal bool expression ----- */

  def bindLiteralBool(
      literalBool: LiteralBool,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralBool =
    literalBool match {
      case factor: LiteralBoolFactor => factor
      case word: LiteralBoolWord     => bindLiteralBoolWord(word, parents, index)
      case unit: LiteralBoolUnit     => bindLiteralBoolUnit(unit, parents, index)
      case group: LiteralBoolGroup   => bindLiteralBoolGroup(group, parents, index)
    }

  def bindLiteralBoolWord(
      word: LiteralBoolWord,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralBool =
    indexLookup(word.value, word.pos, parents, index).asInstanceOf[LiteralBool]

  @scala.annotation.tailrec
  def bindLiteralBoolUnit(
      unit: LiteralBoolUnit,
      parents: List[WordToken],
      index: List[IndexRow]
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
      parents: List[WordToken],
      index: List[IndexRow]
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
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralString =
    literalString match {
      case factor: LiteralStringFactor => factor
      case word: LiteralStringWord     => bindLiteralStringWord(word, parents, index)
      case group: LiteralStringGroup   => bindLiteralStringGroup(group, parents, index)
    }

  def bindLiteralStringWord(
      word: LiteralStringWord,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralString =
    indexLookup(word.value, word.pos, parents, index).asInstanceOf[LiteralString]

  @scala.annotation.tailrec
  def bindLiteralStringGroup(
      group: LiteralStringGroup,
      parents: List[WordToken],
      index: List[IndexRow]
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
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralNumber =
    literalNumber match {
      case factor: LiteralNumberFactor => factor
      case word: LiteralNumberWord     => bindLiteralNumberWord(word, parents, index)
      case group: LiteralNumberGroup   => bindLiteralNumberGroup(group, parents, index)
    }

  def bindLiteralNumberWord(
      word: LiteralNumberWord,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralNumber =
    indexLookup(word.value, word.pos, parents, index).asInstanceOf[LiteralNumber]

  @scala.annotation.tailrec
  def bindLiteralNumberGroup(
      group: LiteralNumberGroup,
      parents: List[WordToken],
      index: List[IndexRow]
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
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralArray =
    LiteralArray(literalArray.items.map(item => bindArrayItem(item, parents, index)))

  def bindArrayItem(
      arrayItem: LiteralExpr,
      parents: List[WordToken],
      index: List[IndexRow]
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
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralObject =
    literalObject.copy(items = bindLiteralObjectItems(literalObject.items, parents, index))

  def bindLiteralObjectItems(
      literalObjectItems: LiteralObjectItems,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralObjectItems =
    LiteralObjectItems(
      literalObjectItems.items.map(item => bindLiteralObjectItem(item, parents, index))
    )

  def bindLiteralObjectItem(
      objectItem: LiteralObjectItem,
      parents: List[WordToken],
      index: List[IndexRow]
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
        item.copy(itemVal = bindLiteralObject(itemVal, name :: parents, index))
      case item @ LiteralObjectItem(name, itemVal: LiteralProto) =>
        item.copy(itemVal = bindLiteralProto(itemVal, name :: parents, index))
    }

  /* ----- literal proto expression ----- */

  def bindLiteralProto(
      literalProto: LiteralProto,
      parents: List[WordToken],
      index: List[IndexRow]
  ): LiteralProto =
    literalProto.copy(items = bindLiteralObjectItems(literalProto.items, parents, index))

}
