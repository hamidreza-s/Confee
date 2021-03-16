package com.spotify.confee

object ConfeeValidator {
  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] =
    Right(ast)
}
