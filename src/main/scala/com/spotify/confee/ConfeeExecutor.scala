package com.spotify.confee

object ConfeeExecutor {
  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] =
    Right(ast)
}
