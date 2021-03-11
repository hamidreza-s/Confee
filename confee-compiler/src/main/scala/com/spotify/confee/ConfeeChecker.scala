package com.spotify.confee

object ConfeeChecker {
  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] =
    Right(ast)
}
