package com.spotify.confee

object ConfeeCompiler {

  def apply(code: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens <- ConfeeLexer(code).right
      ast <- ConfeeParser(tokens).right
    } yield ast
  }

  def main(args: Array[String]): Unit = {
    val input = "777 + (888 + 999) + (333 + 444 / (555 * 444))"
    val output = ConfeeCompiler(input)

    println(input + " = " + output)
  }

}
