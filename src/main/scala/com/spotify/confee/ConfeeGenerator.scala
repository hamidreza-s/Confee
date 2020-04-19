package com.spotify.confee

object ConfeeGenerator {
  def apply(ast: ConfeeAST): Either[ConfeeError, ConfeeIRC] =
    Right(ConfeeIRC())
}
