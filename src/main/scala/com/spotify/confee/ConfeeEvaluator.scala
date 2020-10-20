package com.spotify.confee

object ConfeeEvaluator {

  def evaluateConfItems(confItems: ConfItems): ConfItems =
    ConfItems(confItems.items.map(evaluateConfItem))

  def evaluateConfItem(confItem: ConfItem): ConfItem = confItem match {
    case item @ ConfItem(_, itemVal: LiteralString) =>
      item.copy(itemVal = evaluateLiteralString(itemVal))
    case otherwise => otherwise
  }

  def evaluateLiteralString(literalString: LiteralString): LiteralStringFactor =
    literalString match {
      case factor: LiteralStringFactor => factor
      case group: LiteralStringGroup   => evaluateLiteralStringGroup(group)
    }

  def evaluateLiteralStringGroup(group: LiteralStringGroup): LiteralStringFactor = group match {
    case LiteralStringGroup(
        operator: LiteralStringOperator,
        left: LiteralStringFactor,
        right: LiteralStringGroup
        ) =>
      evaluateLiteralStringGroup(
        LiteralStringGroup(
          operator = operator,
          left = left,
          right = evaluateLiteralStringGroup(right)
        )
      )
    case LiteralStringGroup(
        operator: LiteralStringOperator,
        left: LiteralStringFactor,
        right: LiteralStringFactor
        ) =>
      operator match {
        case LiteralStringOperatorConcat() =>
          LiteralStringFactor(StringToken(left.value.value + right.value.value))
        case LiteralStringOperatorRemove() =>
          LiteralStringFactor(StringToken(left.value.value.replaceFirst(right.value.value, "")))
      }
  }

  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = {
    ast match {
      case Grammar(stmts: List[Stmt]) =>
        val evaluatedStmts = stmts.map {
          case confStmt @ ConfStmt(_, _, items) => confStmt.copy(items = evaluateConfItems(items))
          case otherwise                        => otherwise
        }

        Right(Grammar(evaluatedStmts))
      case _ =>
        Left(
          ConfeeEvaluatorError(
            Location(ast.pos.line, ast.pos.column),
            "AST does not contain valid grammar structure"
          )
        )
    }
  }
}
