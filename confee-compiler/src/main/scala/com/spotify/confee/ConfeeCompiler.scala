package com.spotify.confee

import java.io.File
import java.net.URI

object ConfeeCompiler {

  sealed trait Target
  case object JSON extends Target
  case object YAML extends Target

  // TODO: Check recursive self imports
  // TODO: Check for duplicate imports
  // TODO: Add tests for ConfeeLinker.Reader
  // TODO: Make sure a conf has all items which is defined in its type
  // TODO: Make parser smarter
  // TODO: Add REST API + GUI for debugging/testing
  // TODO: Conf value should accept array value as well as obj (+ test/readme)
  // TODO: Add "private" keyword for the items you don't want to expose
  // TODO: Add "proto." keyword for proto items to get access to items of referenced objects
  // TODO: Add unit tests for ConfeeHelper
  // TODO: Refactor value names of AST case classes (name, value, items, etc. can be better)
  // TODO: Make compiler/cli compatible with ScalaNative
  // TODO: Add 'list-confs'/'list-types' to ConfeeCompiler + ConfeeCLI + ConfeeRest
  // TODO: Cache index not to create it again when needed for the second time
  // TODO: Improve error message description with better Location information
  // TODO: Use map for typeIndex and confIndex for a better performance
  // TODO: Add more unhappy path tests

  /** Compiler Steps:
    * 1. Lexer: It gets the confee config as string and generates tokens based on lexing patterns [done]
    * 2. Parser: It gets tokens and generates AST based on parser grammar [done]
    * 3. Linker: It iterates parsed AST and link config files based on import statements [done]
    * 4. Validator: It iterates linked AST and checks conf/type/item naming correctness [done]
    * 5. Binder: It iterates linked AST and bind references (variables) it their values [done]
    * 6. Evaluator: It iterates bound AST and evaluates expressions [done]
    * 7. Constructor: It iterates evaluated AST and constructs objects from prototypes [done]
    * 8. Executor: It iterates constructed AST and executes lambdas
    * 9. Checker: It iterates executed AST and checks the types [done]
    * 10. Generator: It gets AST and lints/optimises/etc. it into an IRC
    * 11. Formatter: It gets AST/IRC and formats it into the target config (e.g. JSON, YAML, etc.) [done]
    *
    * Notes:
    *  - ConfIndex is being used in Binder, Constructor and Checker steps
    *  - TypeIndex is just being used in Checker step
    *  - Confee supports typeless config compiling by a flag, then it skips Checker step
    */
  def apply(
      code: String,
      conf: String,
      target: Target,
      include: Seq[File] = Seq.empty[File],
      skipValidating: Boolean = false,
      skipChecking: Boolean = false
  ): Either[ConfeeError, String] =
    for {
      tokens      <- ConfeeLexer(code)
      parsed      <- ConfeeParser(tokens)
      linked      <- ConfeeLinker(parsed, include)
      validated   <- ConfeeValidator(linked)
      bound       <- ConfeeBinder(validated)
      evaluated   <- ConfeeEvaluator(bound)
      constructed <- ConfeeConstructor(evaluated)
      executed    <- ConfeeExecutor(constructed)
      checked     <- ConfeeChecker(executed, skipChecking)
      output      <- ConfeeFormatter(checked, conf, target)
    } yield output
}
