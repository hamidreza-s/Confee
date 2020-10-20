package com.spotify.confee

object ConfeeCompiler {

  sealed trait Target
  case object JSON extends Target
  case object YAML extends Target
  case object SQL extends Target
  case object INI extends Target
  case object PYTHON extends Target
  case object RUBY extends Target

  def apply(code: String, target: Target): Either[ConfeeError, String] =
    // TODO:
    //  - lexer -> tokens [done]
    //  - parser -> ast [done]
    //  - linker (import) -> ast
    //  - binder (reference) -> ast
    //  - evaluator (expression) -> ast [wip]
    //  - executor (lambda) -> ast
    //  - checker (type) -> ast
    //  - generator (lint, optimize, etc.) -> irc
    //  - formatter (target) -> out
    for {
      tokens    <- ConfeeLexer(code).right
      parsed    <- ConfeeParser(tokens).right
      linked    <- ConfeeLinker(parsed).right
      bound     <- ConfeeBinder(linked).right
      evaluated <- ConfeeEvaluator(bound).right
      executed  <- ConfeeExecutor(evaluated).right
      checked   <- ConfeeChecker(executed).right
      generated <- ConfeeGenerator(checked).right
      output    <- ConfeeFormatter(generated, target).right
    } yield output

}
