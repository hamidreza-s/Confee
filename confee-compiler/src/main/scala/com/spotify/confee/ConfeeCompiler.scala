package com.spotify.confee

object ConfeeCompiler {

  sealed trait Target
  case object JSON extends Target
  case object YAML extends Target
  case object SQL extends Target
  case object INI extends Target
  case object PYTHON extends Target
  case object RUBY extends Target

  // TODO: Make binding to work regardless of value definition order [done]
  // TODO: Proto literal must be converted to object in binding/evaluator step [done]
  // TODO: Make it possible to use a conf as object/proto in another conf
  // TODO: Don't let objects and protos have duplicated item name before constructor step
  // TODO: Type should just start with Uppercase (fix test and readme)
  // TODO: Conf Value should always start with Lowercase (fix test and readme)
  // TODO: Conf value should accept array value as well as obj
  // TODO: add "private" keyword for the items you don't want to expose
  // TODO: add "proto." keyword for proto items to get access to items of referenced objects
  // TODO: add unit tests for ConfeeHelper/ConfeeIndexer

  /** Compiler Steps:
    * 1. Lexer: It gets the confee config as string and generates tokens based on lexing patterns
    * 2. Parser: It gets tokens and generates AST based on parser grammar
    * 3. Linker: It iterates parsed AST and link config files based on import statements
    * 4. Binder: It iterates linked AST and bind references (variables) it their values
    * 5. Evaluator: It iterates bound AST and evaluates expressions
    * 6. Constructor: It iterates evaluated AST and construct objects from prototypes
    * 7. Executor: It iterates constructed AST and execute lambdas
    * 8. Checker: It iterates executed AST and check types
    * 9. Generator: It gets AST and lints/optimises/etc. it into an IRC
    * 10. Formatter: It gets AST/IRC and formats it into the target config (e.g. JSON, YAML, etc.)
    */
  def apply(code: String, conf: String, target: Target): Either[ConfeeError, String] =
    // TODO:
    //  - lexer -> tokens [done]
    //  - parser -> ast [done]
    //  - linker (import) -> ast [wip]
    //  - binder (reference) -> ast [done]
    //  - evaluator (expression) -> ast [done]
    //  - constructor (proto) -> ast [done]
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
      tokens      <- ConfeeLexer(code).right
      parsed      <- ConfeeParser(tokens).right
      linked      <- ConfeeLinker(parsed).right
      bound       <- ConfeeBinder(linked).right
      evaluated   <- ConfeeEvaluator(bound).right
      constructed <- ConfeeConstructor(evaluated).right
      executed    <- ConfeeExecutor(constructed).right
      checked     <- ConfeeChecker(executed).right
      output      <- ConfeeFormatter(checked, conf, target).right
    } yield output

}
