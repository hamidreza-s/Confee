package com.spotify.confee

import com.spotify.confee.ConfeeCompiler.JSON
import io.circe.Json

import scala.util.{Failure, Success, Try}

object ConfeeFormatter {

  def toJsonLiteralBoolFactor(literalBoolFactor: LiteralBoolFactor): Json =
    Json.fromBoolean(literalBoolFactor.value.value)

  def toJsonLiteralNumberFactor(literalNumberFactor: LiteralNumberFactor): Json =
    Json.fromDoubleOrString(literalNumberFactor.value.value)

  def toJsonLiteralStringFactor(literalStringFactor: LiteralStringFactor): Json =
    Json.fromString(literalStringFactor.value.value)

  def toJsonLiteralArray(literalArray: LiteralArray): Json =
    Json.arr(toJsonLiteralArrayItems(literalArray): _*)

  def toJsonLiteralArrayItems(literalArray: LiteralArray): List[Json] =
    literalArray.items.map {
      case literalBool: LiteralBoolFactor     => toJsonLiteralBoolFactor(literalBool)
      case literalNumber: LiteralNumberFactor => toJsonLiteralNumberFactor(literalNumber)
      case literalString: LiteralStringFactor => toJsonLiteralStringFactor(literalString)
      case literalObject: LiteralObject       => toJsonLiteralObject(literalObject)
      case otherwise: LiteralExpr =>
        throw ConfeeException(
          Location(otherwise.pos.line, otherwise.pos.column),
          "Not allowed literal expression in Formatting step!"
        )
    }

  def toJsonLiteralObject(literalObject: LiteralObject): Json =
    Json.obj(toJsonLiteralObjectItems(literalObject.items): _*)

  def toJsonLiteralObjectItems(literalObjectItems: LiteralObjectItems): List[(String, Json)] =
    literalObjectItems.items.map {
      case LiteralObjectItem(name, itemVal: LiteralBoolFactor) =>
        (name.word, toJsonLiteralBoolFactor(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralNumberFactor) =>
        (name.word, toJsonLiteralNumberFactor(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralStringFactor) =>
        (name.word, toJsonLiteralStringFactor(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralArray) =>
        (name.word, toJsonLiteralArray(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralObject) =>
        (name.word, toJsonLiteralObject(itemVal))
      case LiteralObjectItem(name, _: LiteralExpr) =>
        throw ConfeeException(
          Location(name.pos.line, name.pos.column),
          "Not allowed literal expression in formatting step!"
        )
    }

  def toJson(confStmt: ConfStmt): Either[ConfeeError, String] =
    Try(confStmt.items.items.map {
      case ConfItem(name, itemVal: LiteralBoolFactor) =>
        (name.word, toJsonLiteralBoolFactor(itemVal))
      case ConfItem(name, itemVal: LiteralNumberFactor) =>
        (name.word, toJsonLiteralNumberFactor(itemVal))
      case ConfItem(name, itemVal: LiteralStringFactor) =>
        (name.word, toJsonLiteralStringFactor(itemVal))
      case ConfItem(name, itemVal: LiteralArray) =>
        (name.word, toJsonLiteralArray(itemVal))
      case ConfItem(name, itemVal: LiteralObject) =>
        (name.word, toJsonLiteralObject(itemVal))
      case ConfItem(name, _: LiteralExpr) =>
        throw ConfeeException(
          Location(name.pos.line, name.pos.column),
          "Not allowed literal expression in formatting step!"
        )
    }) match {
      case Success(items)               => Right(Json.obj(items: _*).spaces4)
      case Failure(ex: ConfeeException) => Left(ConfeeEvaluatorError(ex.location, ex.msg))
      case Failure(ex)                  => Left(ConfeeUnknownError(ex))
    }

  def pickConf(ast: ConfeeAST, conf: String): Either[ConfeeError, ConfStmt] = ast match {
    case Grammar(stmts: List[Stmt]) =>
      stmts.find {
        case ConfStmt(WordToken(`conf`), _, _) => true
        case _                                 => false
      } match {
        case Some(confStmt: ConfStmt) => Right(confStmt)
        case _                        => Left(ConfeeNotFoundError(s"The $conf conf is not defined!"))
      }
    case otherwise =>
      Left(
        ConfeeFormatterError(
          Location(otherwise.pos.line, otherwise.pos.column),
          "AST in formatting step does not contain valid grammar structure"
        )
      )
  }

  def formatConf(confStmt: ConfStmt, target: ConfeeCompiler.Target): Either[ConfeeError, String] =
    target match {
      case JSON => toJson(confStmt)
      case _    => Left(ConfeeNotImplementedError(s"$target target is not implemented!"))
    }

  def apply(
      ast: ConfeeAST,
      conf: String,
      target: ConfeeCompiler.Target
  ): Either[ConfeeError, String] = pickConf(ast, conf) match {
    case Right(confStmt) => formatConf(confStmt, target)
    case Left(error)     => Left(error)
  }
}