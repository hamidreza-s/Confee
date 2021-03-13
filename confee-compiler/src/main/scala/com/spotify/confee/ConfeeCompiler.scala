package com.spotify.confee

object ConfeeCompiler {

  sealed trait Target
  case object JSON extends Target
  case object YAML extends Target

  // DONE: Make binding to work regardless of value definition order [done]
  // DONE: Proto literal must be converted to object in binding/evaluator step [done]
  // DONE: Add unit tests for ConfeeIndexer [done]
  // DONE: Make it possible to use a conf as an object in another conf in binder [done]
  // DONE: Make it possible to use a conf as a proto in another conf in constructor (*** priority 1 ***)

  // TODO: Don't let confs, objects and protos have same item name before constructor step
  // TODO: Don't let confs have same name
  // TODO: Type should just start with Uppercase (fix test and readme)
  // TODO: Conf Value should always start with Lowercase (fix test and readme)
  // TODO: Conf value should accept array value as well as obj
  // TODO: Add "private" keyword for the items you don't want to expose
  // TODO: Add "proto." keyword for proto items to get access to items of referenced objects
  // TODO: Add unit tests for ConfeeHelper
  // TODO: Make compiler/cli compatible with ScalaNative
  // TODO: Add REST API + GUI for debugging/testing (*** priority 3 ***)
  // TODO: Implement type checker before binder and then fix the todo items in ConfeeIndexer (*** priority 2 ***)
  // TODO: Add 'list-confs'/'list-types' to ConfeeCompiler + ConfeeCLI + ConfeeRest
  // TODO: Cache index not to create twice in binding and constructing steps

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
    //  - formatter (conf, target) -> yaml [done]
    for {
      tokens      <- ConfeeLexer(code)
      parsed      <- ConfeeParser(tokens)
      linked      <- ConfeeLinker(parsed)
      bound       <- ConfeeBinder(linked)
      evaluated   <- ConfeeEvaluator(bound)
      constructed <- ConfeeConstructor(evaluated)
      executed    <- ConfeeExecutor(constructed)
      checked     <- ConfeeChecker(executed)
      output      <- ConfeeFormatter(checked, conf, target)
    } yield output

}
