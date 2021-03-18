package com.spotify.confee

import com.spotify.confee.ConfeeIndexer.{Index, indexStmts}

object ConfeeChecker {
  def apply(ast: ConfeeAST, skip: Boolean = false): Either[ConfeeError, ConfeeAST] = {
    if (skip) Right(ast)
    else {
      ast match {
        case Grammar(stmts: List[Stmt]) =>
          val typeCheckingErrors = indexStmts(stmts).foldLeft(List.empty[ConfeeCheckerError]) {
            case (acc, Index(_, Left(ConfeeIndexerError(location, msg)))) =>
              ConfeeCheckerError(location, msg) :: acc
            case (acc, _) => acc
          }

          if (typeCheckingErrors.isEmpty) Right(ast)
          else {
            val sortedDistinctErrors = typeCheckingErrors.sortBy { e =>
              (e.location.line, e.location.column)
            }.distinct
            Left(ConfeeCheckerErrors(sortedDistinctErrors))
          }
        case otherwise =>
          Left(
            ConfeeCheckerError(
              Location(otherwise.pos.line, otherwise.pos.column),
              "AST in checker step does not contain valid grammar structure"
            )
          )
      }
    }
  }
}
