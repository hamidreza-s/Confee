package com.spotify.confee

import scala.util.{Failure, Success, Try}

object ConfeeEvaluator {

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      Try(stmts.map {
        case confStmt @ ConfStmt(_, _, items) => confStmt.copy(items = evaluateConfItems(items))
        case otherwise                        => otherwise
      }) match {
        case Success(evaluatedStmts)      => Right(Grammar(evaluatedStmts))
        case Failure(ex: ConfeeException) => Left(ConfeeEvaluatorError(ex.location, ex.msg))
        case Failure(ex)                  => Left(ConfeeUnknownError(ex))
      }
    case otherwise =>
      Left(
        ConfeeEvaluatorError(
          Location(otherwise.pos.line, otherwise.pos.column),
          "AST in evaluation input does not contain valid grammar structure"
        )
      )
  }

  /* ----- evaluate config statement items ----- */

  def evaluateConfItems(confItems: ConfItems): ConfItems =
    ConfItems(confItems.items.map(evaluateConfItem))

  def evaluateConfItem(confItem: ConfItem): ConfItem = confItem match {
    case item @ ConfItem(_, itemVal: LiteralBool) =>
      item.copy(itemVal = evaluateLiteralBool(itemVal))
    case item @ ConfItem(_, itemVal: LiteralString) =>
      item.copy(itemVal = evaluateLiteralString(itemVal))
    case item @ ConfItem(_, itemVal: LiteralNumber) =>
      item.copy(itemVal = evaluateLiteralNumber(itemVal))
    case item @ ConfItem(_, itemVal: LiteralArray) =>
      item.copy(itemVal = evaluateLiteralArray(itemVal))
    case item @ ConfItem(_, itemVal: LiteralObject) =>
      item.copy(itemVal = evaluateLiteralObject(itemVal))
    case item @ ConfItem(_, itemVal: LiteralProto) =>
      item.copy(itemVal = evaluateLiteralProto(itemVal))
  }

  /* ----- literal bool expression ----- */

  def evaluateLiteralBool(literalBool: LiteralBool): LiteralBoolFactor = literalBool match {
    case factor: LiteralBoolFactor => factor
    case word: LiteralBoolWord     => evaluateLiteralBoolWord(word)
    case unit: LiteralBoolUnit     => evaluateLiteralBoolUnit(unit)
    case group: LiteralBoolGroup   => evaluateLiteralBoolGroup(group)
  }

  def evaluateLiteralBoolWord(word: LiteralBoolWord): LiteralBoolFactor =
    throw ConfeeException(
      Location(word.pos.line, word.pos.column),
      "Literal Bool Word must have been referenced in binder step"
    )

  @scala.annotation.tailrec
  def evaluateLiteralBoolUnit(unit: LiteralBoolUnit): LiteralBoolFactor = unit match {
    case LiteralBoolUnit(operator, unit: LiteralBoolFactor) =>
      operator match {
        case LiteralBoolOperatorNot() =>
          LiteralBoolFactor(BoolToken(!unit.value.value))
      }
    case LiteralBoolUnit(operator, unit) =>
      evaluateLiteralBoolUnit(
        LiteralBoolUnit(
          operator = operator,
          unit = evaluateLiteralBool(unit)
        )
      )
  }

  @scala.annotation.tailrec
  def evaluateLiteralBoolGroup(group: LiteralBoolGroup): LiteralBoolFactor = group match {
    case LiteralBoolGroup(operator, left: LiteralBoolFactor, right: LiteralBoolFactor) =>
      operator match {
        case LiteralBoolOperatorAnd() =>
          LiteralBoolFactor(BoolToken(left.value.value & right.value.value))
        case LiteralBoolOperatorOr() =>
          LiteralBoolFactor(BoolToken(left.value.value | right.value.value))
        case LiteralBoolOperatorXor() =>
          LiteralBoolFactor(BoolToken(left.value.value ^ right.value.value))
      }
    case LiteralBoolGroup(operator, left, right) =>
      evaluateLiteralBoolGroup(
        LiteralBoolGroup(
          operator = operator,
          left = evaluateLiteralBool(left),
          right = evaluateLiteralBool(right)
        )
      )
  }

  /* ----- literal string expression ----- */

  def evaluateLiteralString(literalString: LiteralString): LiteralStringFactor =
    literalString match {
      case factor: LiteralStringFactor => factor
      case word: LiteralStringWord     => evaluateLiteralStringWord(word)
      case group: LiteralStringGroup   => evaluateLiteralStringGroup(group)
    }

  def evaluateLiteralStringWord(word: LiteralStringWord): LiteralStringFactor =
    throw ConfeeException(
      Location(word.pos.line, word.pos.column),
      "Literal String Word must have been referenced in binder step"
    )

  @scala.annotation.tailrec
  def evaluateLiteralStringGroup(group: LiteralStringGroup): LiteralStringFactor = group match {
    case LiteralStringGroup(operator, left: LiteralStringFactor, right: LiteralStringFactor) =>
      operator match {
        case LiteralStringOperatorConcat() =>
          LiteralStringFactor(StringToken(left.value.value + right.value.value))
        case LiteralStringOperatorRemove() =>
          LiteralStringFactor(StringToken(left.value.value.replaceFirst(right.value.value, "")))
      }
    case LiteralStringGroup(operator, left, right) =>
      evaluateLiteralStringGroup(
        LiteralStringGroup(
          operator = operator,
          left = evaluateLiteralString(left),
          right = evaluateLiteralString(right)
        )
      )
  }

  /* ----- literal number expression ----- */

  def evaluateLiteralNumber(literalNumber: LiteralNumber): LiteralNumberFactor =
    literalNumber match {
      case factor: LiteralNumberFactor => factor
      case word: LiteralNumberWord     => evaluateLiteralNumberWord(word)
      case group: LiteralNumberGroup   => evaluateLiteralNumberGroup(group)
    }

  def evaluateLiteralNumberWord(word: LiteralNumberWord): LiteralNumberFactor =
    throw ConfeeException(
      Location(word.pos.line, word.pos.column),
      "Literal Number Word must have been referenced in binder step"
    )

  @scala.annotation.tailrec
  def evaluateLiteralNumberGroup(group: LiteralNumberGroup): LiteralNumberFactor = group match {
    case LiteralNumberGroup(operator, left: LiteralNumberFactor, right: LiteralNumberFactor) =>
      operator match {
        case LiteralNumberOperatorAdd() =>
          LiteralNumberFactor(NumberToken(left.value.value + right.value.value))
        case LiteralNumberOperatorSub() =>
          LiteralNumberFactor(NumberToken(left.value.value - right.value.value))
        case LiteralNumberOperatorMul() =>
          LiteralNumberFactor(NumberToken(left.value.value * right.value.value))
        case LiteralNumberOperatorDiv() =>
          LiteralNumberFactor(NumberToken(left.value.value / right.value.value))
        case LiteralNumberOperatorMod() =>
          LiteralNumberFactor(NumberToken(left.value.value % right.value.value))
      }
    case LiteralNumberGroup(operator, left, right) =>
      evaluateLiteralNumberGroup(
        LiteralNumberGroup(
          operator = operator,
          left = evaluateLiteralNumber(left),
          right = evaluateLiteralNumber(right)
        )
      )
  }

  /* ----- literal array expression ----- */

  def evaluateLiteralArray(literalArray: LiteralArray): LiteralArray =
    LiteralArray(literalArray.items.map(evaluateArrayItem))

  def evaluateArrayItem(arrayItem: LiteralExpr): LiteralExpr = arrayItem match {
    case literalBool: LiteralBool     => evaluateLiteralBool(literalBool)
    case literalString: LiteralString => evaluateLiteralString(literalString)
    case literalNumber: LiteralNumber => evaluateLiteralNumber(literalNumber)
    case literalArray: LiteralArray   => evaluateLiteralArray(literalArray)
    case literalObject: LiteralObject => evaluateLiteralObject(literalObject)
    case literalProto: LiteralProto   => evaluateLiteralProto(literalProto)
  }

  /* ----- literal object expression ----- */

  def evaluateLiteralObject(literalObject: LiteralObject): LiteralObject =
    literalObject.copy(items = evaluateLiteralObjectItems(literalObject.items))

  def evaluateLiteralObjectItems(literalObjectItems: LiteralObjectItems): LiteralObjectItems =
    LiteralObjectItems(literalObjectItems.items.map(evaluateObjectItem))

  def evaluateObjectItem(objectItem: LiteralObjectItem): LiteralObjectItem = objectItem match {
    case item @ LiteralObjectItem(_, itemVal: LiteralBool) =>
      item.copy(itemVal = evaluateLiteralBool(itemVal))
    case item @ LiteralObjectItem(_, itemVal: LiteralString) =>
      item.copy(itemVal = evaluateLiteralString(itemVal))
    case item @ LiteralObjectItem(_, itemVal: LiteralNumber) =>
      item.copy(itemVal = evaluateLiteralNumber(itemVal))
    case item @ LiteralObjectItem(_, itemVal: LiteralArray) =>
      item.copy(itemVal = evaluateLiteralArray(itemVal))
    case item @ LiteralObjectItem(_, itemVal: LiteralObject) =>
      item.copy(itemVal = evaluateLiteralObject(itemVal))
    case item @ LiteralObjectItem(_, itemVal: LiteralProto) =>
      item.copy(itemVal = evaluateLiteralProto(itemVal))
  }

  /* ----- literal proto expression ----- */

  def evaluateLiteralProto(literalProto: LiteralProto): LiteralProto =
    literalProto.copy(items = evaluateLiteralObjectItems(literalProto.items))
}
