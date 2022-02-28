package com.spotify.confee

import com.spotify.confee.ConfeeIndexer._
import com.spotify.confee.Location.fromPosition

import scala.util.parsing.input.Position

// TODO: check if object/conf fields has the correct type [WIP]
// TODO: check if there is a field in object/conf which has not been defined in its type [DONE]
// TODO: check if a conf has correct type [WIP]
// TODO: write test for all unhappy paths [WIP]

object ConfeeChecker {

  def checkBoolTypeError(
      name: String,
      pos: Position,
      isArrayItem: Boolean,
      definedType: Option[DefinedType]
  ): List[ConfeeCheckerError] =
    definedType match {
      case Some(BoolDefinedType(_, _)) => Nil
      case Some(df)                    => List(invalidTypeError(name, pos, isArrayItem, df))
      case None                        => List(invalidFieldError(name, pos))
    }

  def checkNumberTypeError(
      name: String,
      pos: Position,
      isArrayItem: Boolean,
      definedType: Option[DefinedType]
  ): List[ConfeeCheckerError] =
    definedType match {
      case Some(NumberDefinedType(_, _)) => Nil
      case Some(df)                      => List(invalidTypeError(name, pos, isArrayItem, df))
      case None                          => List(invalidFieldError(name, pos))
    }

  def checkStringTypeError(
      name: String,
      pos: Position,
      isArrayItem: Boolean,
      definedType: Option[DefinedType]
  ): List[ConfeeCheckerError] =
    definedType match {
      case Some(StringDefinedType(_, _)) => Nil
      case Some(df)                      => List(invalidTypeError(name, pos, isArrayItem, df))
      case None                          => List(invalidFieldError(name, pos))
    }

  def checkArrayTypeErrors(
      name: String,
      items: List[LiteralExpr],
      definedType: Option[DefinedType]
  ): List[ConfeeCheckerError] = {
    definedType match {
      case Some(df) if df.isList => Nil
      case Some(_)               => items.map(item => invalidArrayError(name, item.pos))
      case None                  => items.map(item => invalidFieldError(name, item.pos))
    }
  }

  private def invalidFieldError(name: String, pos: Position) =
    ConfeeCheckerError(
      fromPosition(pos),
      s"Type error: '$name' field name is not compatible with its type"
    )

  private def invalidTypeError(
      name: String,
      pos: Position,
      isArrayItem: Boolean,
      definedType: DefinedType
  ) = {
    val definedTypeName = if (isArrayItem) s"[${definedType.name}]" else s"${definedType.name}"
    ConfeeCheckerError(
      fromPosition(pos),
      s"Type error: '$name' value must have $definedTypeName type"
    )
  }

  private def invalidArrayError(name: String, pos: Position) =
    ConfeeCheckerError(
      fromPosition(pos),
      s"Type error: '$name' value must not be inside an array"
    )

  def checkPrimitiveTypeErrors(
      confIndex: ConfIndex,
      typeIndex: TypeIndex
  ): List[ConfeeCheckerError] = {
    val isArrayItem = confIndex.isArrayItem
    val name        = if (isArrayItem) confIndex.name.split('.').head else confIndex.name
    val pos         = confIndex.expr.pos
    val definedType = typeIndex.items.get(name)
    confIndex.expr match {
      case _: LiteralBoolFactor   => checkBoolTypeError(name, pos, isArrayItem, definedType)
      case _: LiteralNumberFactor => checkNumberTypeError(name, pos, isArrayItem, definedType)
      case _: LiteralStringFactor => checkStringTypeError(name, pos, isArrayItem, definedType)
      case LiteralArray(items)    => checkArrayTypeErrors(name, items, definedType)
      case _                      => Nil
    }
  }

  def apply(ast: ConfeeAST, skip: Boolean = false): Either[ConfeeError, ConfeeAST] = {
    if (skip) Right(ast)
    else {
      ast match {
        case Grammar(stmts: List[Stmt]) =>
          val index = indexStmts(stmts)
          val typeCheckingErrors = index.foldLeft(List.empty[ConfeeCheckerError]) {
            case (acc, Index(_, Left(ConfeeIndexerError(location, msg)))) =>
              ConfeeCheckerError(location, msg) :: acc
            case (acc, Index(confIndex, Right(typeIndex))) =>
              checkPrimitiveTypeErrors(
                confIndex = confIndex,
                typeIndex = typeIndex
              ) ::: acc
          }

          if (typeCheckingErrors.isEmpty) Right(ast)
          else {
            val sortedDistinctErrors = typeCheckingErrors.sortBy { e =>
              (e.location.line, e.location.column)
            }.distinct
            Left(ConfeeCheckerErrors(sortedDistinctErrors))
          }
        case otherwise =>
          Left(
            ConfeeCheckerError(
              Location(otherwise.pos.line, otherwise.pos.column),
              "AST in checker step does not contain valid grammar structure"
            )
          )
      }
    }
  }
}
