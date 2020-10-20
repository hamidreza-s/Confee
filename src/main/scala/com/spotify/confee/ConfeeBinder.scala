package com.spotify.confee

object ConfeeBinder {
  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeAST] =
    Right(ast)
}
