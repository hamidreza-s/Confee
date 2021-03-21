package com.spotify.confee

import com.spotify.confee.Location.fromPosition

import java.io.File
import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import scala.io.Source

object ConfeeLinker {

  case class Code(path: String, source: String)

  case class Reader(
      localImports: Seq[File] = Seq.empty[File],
      remoteImports: Seq[URI] = Seq.empty[URI]
  ) {

    def readLocalFile(name: String): Option[Code] =
      localImports
        .collectFirst {
          case dir: File if dir.isDirectory =>
            dir
              .list()
              .find(_.equals(name))
              .map(file => s"$dir/$file")
        }
        .flatten
        .flatMap { file =>
          val fileSource = Source.fromFile(file)
          val inputLines = fileSource.getLines().mkString("\n")
          fileSource.close()
          Some(Code(file, inputLines))
        }

    def readRemoteFile(name: String): Option[Code] =
      remoteImports.collectFirst {
        case base: URI =>
          val host = Option(base.getHost)
            .map(host => s"$host/${base.getPath}")
            .getOrElse(base.getPath)
          val uri      = s"${host}/${name}"
          val client   = HttpClient.newHttpClient()
          val request  = HttpRequest.newBuilder(new URI(uri)).build()
          val response = client.send(request, BodyHandlers.ofString())

          if (response.statusCode() == 200) Some(Code(uri, response.body()))
          else None
      }.flatten
  }

  def apply(
      ast: ConfeeAST,
      localImports: Seq[File] = Seq.empty[File],
      remoteImports: Seq[URI] = Seq.empty[URI]
  ): Either[ConfeeError, ConfeeAST] = this(ast, Reader(localImports, remoteImports))

  def apply(
      ast: ConfeeAST,
      reader: Reader
  ): Either[ConfeeError, ConfeeAST] = ast match {
    case grammar @ Grammar(stmts: List[Stmt]) =>
      val parsedImports =
        stmts.foldLeft(List.empty[Either[ConfeeError, ConfeeAST]]) {
          case (acc, importStmt: ImportStmt) =>
            importStmtToAst(importStmt, reader) :: acc
          case (acc, _) => acc
        }

      val importedStmts = parsedImports.flatMap {
        case Right(value: Grammar) => value.stmts
        case _                     => Nil
      }

      val errors = parsedImports.collect { case Left(e) => e }
      if (errors.nonEmpty) {
        Left(ConfeeErrors(errors))
      } else {
        Right(grammar.copy(stmts = importedStmts ::: stmts).setPos(grammar.pos))
      }

    case otherwise =>
      Left(
        ConfeeLinkerError(
          Location(otherwise.pos.line, otherwise.pos.column),
          "AST in linker step does not contain valid grammar structure"
        )
      )
  }

  private def importStmtToAst(
      importStmt: ImportStmt,
      reader: Reader
  ): Either[ConfeeError, ConfeeAST] = importStmt match {
    case ImportStmt(StringToken(value)) =>
      reader.readLocalFile(value).orElse(reader.readRemoteFile(value)) match {
        case Some(file) => fileToAst(file, reader)
        case None =>
          Left(
            ConfeeLinkerError(
              fromPosition(importStmt.pos),
              s"Import path ${value} is not available"
            )
          )
      }
  }

  private def fileToAst(
      file: Code,
      reader: Reader
  ): Either[ConfeeError, ConfeeAST] =
    (for {
      tokens <- ConfeeLexer(file.source)
      parsed <- ConfeeParser(tokens)
      linked <- ConfeeLinker(parsed, reader)
    } yield linked).map(_.setPath(file.path))
}
