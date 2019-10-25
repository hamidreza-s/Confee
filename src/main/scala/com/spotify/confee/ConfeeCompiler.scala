package com.spotify.confee

object ConfeeCompiler {

  def apply(code: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens <- ConfeeLexer(code).right
      ast <- ConfeeParser(tokens).right
    } yield ast
  }

}
