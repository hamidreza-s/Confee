package com.spotify.confee

object ConfeeEvaluator {
  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] = {
    ast match {
      case Grammar(stmts: List[Stmt]) =>
        val evaluatedStmts = stmts.map {
          // TODO: use recursion and evaluate expression in place
          case conf: ConfStmt => conf
          case otherwise      => otherwise
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
