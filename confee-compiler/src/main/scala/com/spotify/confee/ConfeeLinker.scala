package com.spotify.confee

object ConfeeLinker {
  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] =
    Right(ast)
}
