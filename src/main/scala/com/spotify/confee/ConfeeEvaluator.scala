package com.spotify.confee

import scala.util.{Failure, Success, Try}

object ConfeeEvaluator {

  /* ----- literal bool expression ----- */

  def evaluateLiteralBool(literalBool: LiteralBool): LiteralBool =
    // TODO: add bitwise operation evaluation
    literalBool

  /* ----- literal string expression ----- */

  def evaluateLiteralString(literalString: LiteralString): LiteralStringFactor =
    literalString match {
      case factor: LiteralStringFactor => factor
      case word: LiteralStringWord     => evaluateLiteralStringWord(word)
      case group: LiteralStringGroup   => evaluateLiteralStringGroup(group)
    }

  def evaluateLiteralStringWord(word: LiteralStringWord): LiteralStringFactor =
    throw new Exception("Literal String Word must have been referenced in binder step")

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
    throw new Exception("Literal Number Word must have been referenced in binder step")

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
    case bool: LiteralBool     => evaluateLiteralBool(bool)
    case string: LiteralString => evaluateLiteralString(string)
    case number: LiteralNumber => evaluateLiteralNumber(number)
    case array: LiteralArray   => evaluateLiteralArray(array)
    case _: LiteralObject      => ???
    case _: LiteralProto       => ???
  }

  /* ----- evaluator entry on config item values ----- */

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
    case otherwise => otherwise
  }

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = {
    ast match {
      case Grammar(stmts: List[Stmt]) =>
        Try(stmts.map {
          case confStmt @ ConfStmt(_, _, items) => confStmt.copy(items = evaluateConfItems(items))
          case otherwise                        => otherwise
        }) match {
          case Success(evaluatedStmts) =>
            Right(Grammar(evaluatedStmts))
          case Failure(exception) =>
            Left(ConfeeEvaluatorError(exception.getMessage))
        }
      case _ =>
        Left(ConfeeEvaluatorError("AST does not contain valid grammar structure"))
    }
  }

}
