package com.spotify.confee

import com.spotify.confee.ConfeeHelper.hasReference

import scala.util.parsing.input.Position

object ConfeeIndexer {

  // TODO (1): Add type information of expressions defined in type statement when indexing
  //           and use it when looking up
  // TODO (2): LiteralExpr/AnyType should be replaced by the type of expression type it is
  //           referring to, because it might be a Bool, Number, String, Array, or Object
  // TODO (3): When (1) and (2) are done, set the `hasSameType` condition to be true for all cases

  /**
    * @param name           the name of the expression
    * @param parents        list of parents' name of the expression
    * @param expr           the expression
    * @param hasReference   if there is a reference inside the expression
    * @param isTopLevel     if the expression is a conf which is defined in top-level
    */
  case class IndexRow(
      name: WordToken,
      parents: List[WordToken] = List.empty[WordToken],
      expr: Expr,
      exprType: ExprType,
      hasReference: Boolean,
      isTopLevel: Boolean = false
  )

  sealed abstract class ExprType(val name: String)
  case object WordType extends ExprType("Word")
  case object BoolType extends ExprType("Bool")
  case object NumberType extends ExprType("Number")
  case object StringType extends ExprType("String")
  case object ArrayType extends ExprType("Array")
  case object ObjectType extends ExprType("Object")
  case object ProtoType extends ExprType("Proto")
  case object AnyType extends ExprType("Any")

  def indexLookup[T <: Expr](
      name: WordToken,
      exprType: ExprType,
      pos: Position,
      parents: List[WordToken],
      index: List[IndexRow]
  ): T =
    index
      .foldLeft(List.empty[(Int, Int, IndexRow)]) {
        case (acc, indexRow) =>
          val equalityHighPriority     = 0
          val equalityMediumPriority   = 1
          val equalityLowPriority      = 2
          val proximityPriority        = parents.size - indexRow.parents.size
          val hasSameName              = indexRow.name.word.equals(name.word)
          val hasSameType              = indexRow.exprType.equals(exprType)
          val hasSameExactParents      = indexRow.parents.equals(parents)
          val hasSameUpperLevelParents = indexRow.parents.forall(parents.contains)

          (hasSameName, hasSameType, hasSameExactParents, hasSameUpperLevelParents) match {
            case (true, true, true, _) => (equalityHighPriority, proximityPriority, indexRow) :: acc
            case (true, true, _, _)    => (equalityMediumPriority, proximityPriority, indexRow) :: acc
            case (true, _, _, true)    => (equalityLowPriority, proximityPriority, indexRow) :: acc
            case _                     => acc
          }
      }
      .sortBy {
        case (parentalEqualityPriority, parentalProximityPriority, _) =>
          (parentalEqualityPriority, parentalProximityPriority)
      }
      .map {
        case (_, _, indexRow) => indexRow
      }
      .headOption match {
      case Some(indexRow) =>
        if (indexRow.hasReference) {
          throw ConfeeException(
            Location(pos.line, pos.column),
            s"Reference error: '${name.word}' has a circular reference"
          )
        } else {
          indexRow.expr.asInstanceOf[T]
        }
      case None =>
        throw ConfeeException(
          Location(pos.line, pos.column),
          s"Reference error: '${name.word}' is not defined"
        )
    }

  def confStmtToExpr(confStmt: ConfStmt): Expr =
    LiteralObject(LiteralObjectItems(confStmt.items.items.map {
      case ConfItem(name, itemVal: LiteralExpr) => LiteralObjectItem(name, itemVal)
    }))

  /* ----- index conf statements and conf item expressions ----- */

  def indexStmts(stmts: List[Stmt]): List[IndexRow] = stmts.flatMap {
    case confStmt: ConfStmt => indexConfStmt(confStmt)
    case _                  => None
  }

  def indexConfStmt(confStmt: ConfStmt): List[IndexRow] = confStmt match {
    case ConfStmt(name, _, items) =>
      /**
        *  In order to index a confStmt, it needs to be converted to an object literal expression.
        *  Then it will be indexed as a top-level object in the index list, so it can be referenced
        *  inside other confStmts.
        */
      val confExpr = confStmtToExpr(confStmt)
      val ConfStmtIndexRow = IndexRow(
        name,
        List.empty[WordToken],
        confExpr,
        ObjectType,
        hasReference(confExpr),
        isTopLevel = true
      )
      items.items
        .foldLeft(List(ConfStmtIndexRow)) {
          case (acc, item) =>
            indexConfItem(
              name = item.name,
              parents = List(name),
              expr = item.itemVal,
              index = acc
            )
        }
  }

  def indexConfItem(
      name: WordToken,
      expr: Expr,
      parents: List[WordToken] = List.empty[WordToken],
      index: List[IndexRow] = List.empty[IndexRow]
  ): List[IndexRow] = expr match {
    case w: LiteralWord =>
      index ::: IndexRow(name, parents, w, WordType, hasReference(w)) :: Nil
    case b: LiteralBool =>
      index ::: IndexRow(name, parents, b, BoolType, hasReference(b)) :: Nil
    case n: LiteralNumber =>
      index ::: IndexRow(name, parents, n, NumberType, hasReference(n)) :: Nil
    case s: LiteralString =>
      index ::: IndexRow(name, parents, s, StringType, hasReference(s)) :: Nil
    case a: LiteralArray =>
      index ::: IndexRow(name, parents, a, ArrayType, hasReference(a)) :: Nil
    case o @ LiteralObject(items: LiteralObjectItems) =>
      index ::: IndexRow(name, parents, o, ObjectType, hasReference(o)) :: indexObjectItems(
        name,
        items,
        parents
      )
    case p @ LiteralProto(_, items: LiteralObjectItems) =>
      index ::: IndexRow(name, parents, p, ProtoType, hasReference(p)) :: indexObjectItems(
        name,
        items,
        parents
      )
  }

  def indexObjectItems(
      name: WordToken,
      objectItems: LiteralObjectItems,
      parents: List[WordToken]
  ): List[IndexRow] =
    objectItems.items.flatMap { item =>
      indexConfItem(name = item.name, expr = item.itemVal, parents = name :: parents)
    }
}
