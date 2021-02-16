package com.spotify.confee

object ConfeeCompiler {

  sealed trait Target
  case object JSON extends Target
  case object YAML extends Target
  case object SQL extends Target
  case object INI extends Target
  case object PYTHON extends Target
  case object RUBY extends Target

  // TODO: Make binding to work regardless of value definition order
  // TODO: Proto literal must be converted to object in binding/evaluator step
  // TODO: Type should just start with Uppercase (fix test and readme)
  // TODO: Conf Value should always start with Lowercase (fix test and readme)
  // TODO: Conf value should accept array value as well as obj

  def apply(code: String, conf: String, target: Target): Either[ConfeeError, String] =
    // TODO:
    //  - lexer -> tokens [done]
    //  - parser -> ast [done]
    //  - linker (import) -> ast [wip]
    //  - binder (reference) -> ast [done]
    //  - evaluator (expression) -> ast [done]
    //  - executor (lambda) -> ast [wip]
    //  - checker (type) -> ast [wip]
    //  - generator (lint, optimize, etc.) -> irc [skipped]
    //  - formatter (conf, target) -> json [done]
    //  - formatter (conf, target) -> yaml [wip]
    //  - formatter (conf, target) -> sql [wip]
    //  - formatter (conf, target) -> ini [wip]
    //  - formatter (conf, target) -> python [wip]
    //  - formatter (conf, target) -> ruby [wip]
    for {
      tokens    <- ConfeeLexer(code).right
      parsed    <- ConfeeParser(tokens).right
      linked    <- ConfeeLinker(parsed).right
      bound     <- ConfeeBinder(linked).right
      evaluated <- ConfeeEvaluator(bound).right
      executed  <- ConfeeExecutor(evaluated).right
      checked   <- ConfeeChecker(executed).right
      output    <- ConfeeFormatter(checked, conf, target).right
    } yield output

}
