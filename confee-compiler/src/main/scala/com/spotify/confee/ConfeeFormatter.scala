package com.spotify.confee

import com.spotify.confee.ConfeeCompiler.{JSON, Target, YAML}
import io.circe.Json
import io.circe.yaml.syntax._

import scala.util.{Failure, Success, Try}

object ConfeeFormatter {

  /* ----- to yaml formatter ----- */

  def toYaml(confStmt: ConfStmt): Either[ConfeeError, YamlSyntax] = toJson(confStmt).map(_.asYaml)

  /* ----- to json formatter ----- */

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
        throw ConfeeCodeException(
          Location(otherwise.pos.line, otherwise.pos.column),
          "Not allowed literal expression in Formatting step!"
        )
    }

  def toJsonLiteralObject(literalObject: LiteralObject): Json =
    Json.obj(toJsonLiteralObjectItems(literalObject.items): _*)

  def toJsonLiteralObjectItems(literalObjectItems: LiteralObjectItems): List[(String, Json)] =
    literalObjectItems.items.map {
      case LiteralObjectItem(name, itemVal: LiteralBoolFactor) =>
        (name.value, toJsonLiteralBoolFactor(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralNumberFactor) =>
        (name.value, toJsonLiteralNumberFactor(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralStringFactor) =>
        (name.value, toJsonLiteralStringFactor(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralArray) =>
        (name.value, toJsonLiteralArray(itemVal))
      case LiteralObjectItem(name, itemVal: LiteralObject) =>
        (name.value, toJsonLiteralObject(itemVal))
      case LiteralObjectItem(name, _: LiteralExpr) =>
        throw ConfeeCodeException(
          Location(name.pos.line, name.pos.column),
          "Not allowed literal expression in formatting step!"
        )
    }

  def toJson(confStmt: ConfStmt): Either[ConfeeError, Json] =
    Try(confStmt.items.items.map {
      case ConfItem(name, itemVal: LiteralBoolFactor) =>
        (name.value, toJsonLiteralBoolFactor(itemVal))
      case ConfItem(name, itemVal: LiteralNumberFactor) =>
        (name.value, toJsonLiteralNumberFactor(itemVal))
      case ConfItem(name, itemVal: LiteralStringFactor) =>
        (name.value, toJsonLiteralStringFactor(itemVal))
      case ConfItem(name, itemVal: LiteralArray) =>
        (name.value, toJsonLiteralArray(itemVal))
      case ConfItem(name, itemVal: LiteralObject) =>
        (name.value, toJsonLiteralObject(itemVal))
      case ConfItem(name, _: LiteralExpr) =>
        throw ConfeeCodeException(
          Location(name.pos.line, name.pos.column),
          "Not allowed literal expression in formatting step!"
        )
    }) match {
      case Success(items)               => Right(Json.obj(items: _*))
      case Failure(ex: ConfeeCodeException) => Left(ConfeeEvaluatorError(ex.location, ex.msg))
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

  def formatConf(confStmt: ConfStmt, target: Target): Either[ConfeeError, String] =
    target match {
      case JSON => toJson(confStmt).map(_.spaces4)
      case YAML => toYaml(confStmt).map(_.spaces4)
      case _    => Left(ConfeeNotImplementedError(s"$target target is not implemented!"))
    }

  def apply(
      ast: ConfeeAST,
      conf: String,
      target: Target
  ): Either[ConfeeError, String] = pickConf(ast, conf) match {
    case Right(confStmt) => formatConf(confStmt, target)
    case Left(error)     => Left(error)
  }
}
