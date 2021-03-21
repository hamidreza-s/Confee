package com.spotify.confee.cli

import com.spotify.confee.ConfeeCompiler.{JSON, Target, YAML}
import com.spotify.confee._
import scopt.{OptionParser, Read}

import java.io.{File, PrintWriter}
import java.net.URI
import scala.io.Source

case class Config(
    name: Option[String] = None,
    target: Option[Target] = None,
    input: Option[File] = None,
    output: Option[File] = None,
    localImports: Seq[File] = Seq.empty[File],
    remoteImports: Seq[URI] = Seq.empty[URI],
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

    implicit val uriConverter: Read[URI] = Read.reads(new URI(_))

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
      .valueName("<file>")
      .action((x, c) => c.copy(input = Some(x)))
      .text("path to confee input file")

    opt[File]('o', "output")
      .optional()
      .valueName("<file>")
      .action((x, c) => c.copy(output = Some(x)))
      .text("path to confee output file")

    opt[Seq[File]]('l', "locals")
      .optional()
      .valueName("<dir>,...")
      .action((x, c) => c.copy(localImports = x))
      .text("comma-separated local import directories")

    opt[Seq[URI]]('r', "remotes")
      .optional()
      .valueName("<uri>,...")
      .action((x, c) => c.copy(remoteImports = x))
      .text("comma-separated remote import URIs")

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
    case Some(
        Config(
          Some(name),
          Some(target),
          Some(inputPath),
          outputPath,
          localImports,
          remoteImports,
          relax,
          typeless
        )
        ) =>
      compile(name, target, inputPath, outputPath, localImports, remoteImports, relax, typeless) match {
        case Right(result) => println(result)
        case Left(error)   => outputError(error)
      }
    case _ => println("Invalid arguments!")
  }

  private def compile(
      configName: String,
      target: Target,
      input: File,
      output: Option[File],
      localImports: Seq[File],
      remoteImports: Seq[URI],
      relax: Boolean,
      typeless: Boolean
  ): Either[ConfeeError, String] = {
    println(s"""
               |Compiling ...
               |>> target: $target
               |>> input: $input
               |>> output: $output
               |""".stripMargin)

    if (!localImports.forall(_.isDirectory)) {
      Left(ConfeeInvalidArgumentError("Local import directory is not valid!"))
    } else if (!remoteImports.forall(uri => Option(uri.getHost).isDefined)) {
      Left(ConfeeInvalidArgumentError("Remote import URI is not valid!"))
    } else if (!input.exists) {
      Left(ConfeeInvalidArgumentError("Input file does not exist!"))
    } else if (!input.canRead) {
      Left(ConfeeInvalidArgumentError("Input file is not readable!"))
    } else {
      val inputSource = Source.fromFile(input)
      val inputLines  = inputSource.getLines().mkString("\n")
      inputSource.close()

      ConfeeCompiler(inputLines, configName, target, localImports, remoteImports, relax, typeless) match {
        case Right(compiledConfig) =>
          output match {
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
  }

  private def outputError(error: ConfeeError): Unit = error match {
    case ConfeeErrors(es)              => es.foreach(outputError)
    case ConfeeLexerError(l, m)        => printError(l, m)
    case ConfeeParserError(l, m)       => printError(l, m)
    case ConfeeLinkerError(l, m)       => printError(l, m)
    case ConfeeValidatorError(l, m)    => printError(l, m)
    case ConfeeBinderError(l, m)       => printError(l, m)
    case ConfeeEvaluatorError(l, m)    => printError(l, m)
    case ConfeeConstructorError(l, m)  => printError(l, m)
    case ConfeeCheckerError(l, m)      => printError(l, m)
    case ConfeeCheckerErrors(es)       => es.foreach(e => printError(e.location, e.msg))
    case ConfeeValidatorErrors(es)     => es.foreach(e => printError(e.location, e.msg))
    case ConfeeInvalidArgumentError(m) => println(s"Invalid argument: $m")
    case e                             => println(s"Compiler error: $e")
  }
  private def printError(l: Location, m: String): Unit = println(s"$m ($l)")
}
