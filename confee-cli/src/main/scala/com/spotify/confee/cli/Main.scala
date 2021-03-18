package com.spotify.confee.cli

import com.spotify.confee.ConfeeCompiler.{JSON, Target, YAML}
import com.spotify.confee._
import scopt.{OptionParser, Read}

import java.io.{File, PrintWriter}
import scala.io.Source

case class Config(
    name: Option[String] = None,
    target: Option[Target] = None,
    input: Option[File] = None,
    output: Option[File] = None,
    relax: Boolean = false,
    typesafe: Boolean = false
)

object Main extends App {

  val parser: OptionParser[Config] = new scopt.OptionParser[Config]("confeec") {

    override def showUsageOnError: Option[Boolean] = Some(true)

    implicit val targetConverter: Read[Target] = Read.reads { t =>
      t.toUpperCase() match {
        case "JSON" => JSON
        case "YAML" => YAML
      }
    }

    head("confeec - Confee Compiler", "0.0.1")

    opt[String]('c', "config")
      .valueName("<name>")
      .action((x, c) => c.copy(name = Some(x)))
      .text("config name to be formatted")

    opt[Target]('t', "target")
      .valueName("json|yaml")
      .action((x, c) => c.copy(target = Some(x)))
      .text("target format of confee file")

    opt[File]('i', "input")
      .valueName("<path>")
      .action((x, c) => c.copy(input = Some(x)))
      .text("path to confee input file")

    opt[File]('o', "output")
      .optional()
      .valueName("<path>")
      .action((x, c) => c.copy(output = Some(x)))
      .text("path to confee output file (optional)")

    opt[Unit]('R', "relax")
      .optional()
      .action((_, c) => c.copy(relax = true))
      .text("disable object key name validator")

    opt[Unit]('T', "typeless")
      .optional()
      .action((_, c) => c.copy(typesafe = true))
      .text("disable type checker")

    help("help").text("prints this usage text")
  }

  parser.parse(args, Config()) match {
    case Some(Config(Some(name), Some(target), Some(inputPath), outputPath, relax, typeless)) =>
      compile(name, target, inputPath, outputPath, relax, typeless) match {
        case Right(result)                      => println(result)
        case Left(ConfeeLexerError(l, m))       => printError(l, m)
        case Left(ConfeeParserError(l, m))      => printError(l, m)
        case Left(ConfeeValidatorError(l, m))   => printError(l, m)
        case Left(ConfeeBinderError(l, m))      => printError(l, m)
        case Left(ConfeeEvaluatorError(l, m))   => printError(l, m)
        case Left(ConfeeConstructorError(l, m)) => printError(l, m)
        case Left(ConfeeCheckerError(l, m))     => printError(l, m)
        case Left(ConfeeCheckerErrors(es))      => es.map(e => printError(e.location, e.msg))
        case Left(ConfeeValidatorErrors(es))    => es.map(e => printError(e.location, e.msg))
        case Left(error)                        => println(s"Compiler: $error")
      }
    case _ => println("Invalid arguments!")
  }

  private def compile(
      configName: String,
      target: Target,
      input: File,
      maybeOutput: Option[File],
      relax: Boolean,
      typeless: Boolean
  ): Either[ConfeeError, String] = {
    println(s"""
               |Compiling ...
               |>> target: $target
               |>> input: $input
               |>> output: $maybeOutput
               |""".stripMargin)

    if (!input.exists)
      throw new Exception("Input file does not exist!")

    if (!input.canRead)
      throw new Exception("Input file is not readable!")

    val inputSource = Source.fromFile(input)
    val inputLines  = inputSource.getLines().mkString("\n")
    inputSource.close()

    ConfeeCompiler(inputLines, configName, target, relax, typeless) match {
      case Right(compiledConfig) =>
        maybeOutput match {
          case Some(outputFilePath) =>
            val outputFile = new PrintWriter(outputFilePath)
            outputFile.write(compiledConfig)
            outputFile.close()
            Right("Compiled Successfully!")

          case None => Right(compiledConfig)
        }
      case otherwise => otherwise
    }
  }

  private def printError(l: Location, m: String): Unit = println(s"$m ($l)")
}
