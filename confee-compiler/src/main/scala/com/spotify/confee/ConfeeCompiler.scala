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
  // DONE: Type name should just start with Uppercase (+ test/readme)
  // DONE: Conf name should always start with Lowercase (+ test/readme)
  // DONE: All type/conf/object/proto item keys can be both uppercase and lowercase

  // TODO: Type Checker/Indexer: (*** priority 2 ***)
  // - Index defined types in Indexer [done]
  // - Add defined type (next to already existing inferred types) to index row
  // - Implement type checker before binder
  // - Fix the to-do items in ConfeeIndexer
  // - Don't let having wrong types in confs
  // - Don't let confs, objects and protos have same item name before constructor step
  // - Don't let confs have same name
  // - Don't let types have same name
  // TODO: Conf value should accept array value as well as obj (+ test/readme) (*** priority 4 ***)
  // TODO: Add "private" keyword for the items you don't want to expose
  // TODO: Add "proto." keyword for proto items to get access to items of referenced objects
  // TODO: Add unit tests for ConfeeHelper
  // TODO: Refactor value names of AST case classes (name, value, items, etc. can be better)
  // TODO: Make compiler/cli compatible with ScalaNative
  // TODO: Add REST API + GUI for debugging/testing (*** priority 3 ***)
  // TODO: Add 'list-confs'/'list-types' to ConfeeCompiler + ConfeeCLI + ConfeeRest
  // TODO: Cache index not to create twice in binding and constructing steps
  // TODO: Improve error message description with better Location information

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
