package com.spotify.confee.cli

import com.spotify.confee.ConfeeCompiler
import com.spotify.confee.ConfeeCompiler.{JSON, Target, YAML}
import scopt.{OptionParser, Read}

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class Config(
    name: Option[String] = None,
    target: Option[Target] = None,
    input: Option[File] = None,
    output: Option[File] = None
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
      .valueName("JSON|YAML")
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

    help("help").text("prints this usage text")
  }

  Try(parser.parse(args, Config()) match {
    case Some(Config(Some(name), Some(target), Some(inputFilePath), maybeOutputFilePath)) =>
      compile(name, target, inputFilePath, maybeOutputFilePath)
    case _ => throw new Exception("Invalid arguments!")
  }) match {
    case Failure(exception) => println(s"Failure: ${exception.getMessage}")
    case Success(output)    => println(output)
  }

  private def compile(
      configName: String,
      target: Target,
      input: File,
      maybeOutput: Option[File]
  ): String = {
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

    ConfeeCompiler(inputLines, configName, target) match {
      case Right(compiledConfig) =>
        maybeOutput match {
          case Some(outputFilePath) =>
            val outputFile = new PrintWriter(outputFilePath)
            outputFile.write(compiledConfig)
            outputFile.close()
            "Compiled Successfully!"

          case None => compiledConfig
        }
      case Left(error) => throw new Exception(error.toString)
    }

  }
}
