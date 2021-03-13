package com.spotify.confee

import com.spotify.confee.ConfeeHelper.hasReference

import scala.util.parsing.input.Position

object ConfeeIndexer {

  // TODO (1): Add type information of expressions defined in type statement when indexing
  //           and use it when looking up
  // TODO (2): LiteralExpr/AnyType should be replaced by the type of expression type it is
  //           referring to, because it might be a Bool, Number, String, Array, or Object
  // TODO (3): When (1) and (2) are done, set the `hasSameType` condition to be true for all cases

  case class TypeIndex(
      name: String,
      itemKey: String,
      itemVal: String,
      itemType: DefinedType,
      isList: Boolean
  )

  class DefinedType()
  object DefinedType {
    def parse(name: String): DefinedType = name match {
      case "Bool"   => BoolDefinedType
      case "Number" => NumberDefinedType
      case "String" => StringDefinedType
      case _        => ObjectDefinedType
    }
  }
  case object BoolDefinedType extends DefinedType
  case object NumberDefinedType extends DefinedType
  case object StringDefinedType extends DefinedType
  case object ObjectDefinedType extends DefinedType

  case class ConfIndex(
      name: String,
      parents: List[String] = List.empty[String],
      expr: Expr,
      exprType: InferredType,
      hasReference: Boolean,
      isTopLevel: Boolean = false
  )

  sealed trait InferredType
  case object WordInferredType extends InferredType
  case object BoolInferredType extends InferredType
  case object NumberInferredType extends InferredType
  case object StringInferredType extends InferredType
  case object ArrayInferredType extends InferredType
  case object ObjectInferredType extends InferredType
  case object ProtoInferredType extends InferredType
  case object NoInferredType extends InferredType

  def indexLookup[T <: Expr](
      name: String,
      exprType: InferredType,
      pos: Position,
      parents: List[String],
      index: List[ConfIndex]
  ): T =
    index
      .foldLeft(List.empty[(Int, Int, ConfIndex)]) {
        case (acc, indexRow) =>
          val equalityHighPriority     = 0
          val equalityMediumPriority   = 1
          val equalityLowPriority      = 2
          val proximityPriority        = parents.size - indexRow.parents.size
          val hasSameName              = indexRow.name.equals(name)
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
            s"Reference error: '$name' has a circular reference"
          )
        } else {
          indexRow.expr.asInstanceOf[T]
        }
      case None =>
        throw ConfeeException(
          Location(pos.line, pos.column),
          s"Reference error: '$name' is not defined"
        )
    }

  def confStmtToExpr(confStmt: ConfStmt): Expr =
    LiteralObject(LiteralObjectItems(confStmt.items.items.map {
      case ConfItem(name, itemVal: LiteralExpr) =>
        LiteralObjectItem(LiteralObjectItemKey(name.value), itemVal)
    }))

  /* ----- index type statements ----- */

  def indexTypeStmts(stmts: List[Stmt]): List[TypeIndex] = stmts.flatMap {
    case typeStmt: TypeStmt => indexTypeStmt(typeStmt)
    case _                  => None
  }

  def indexTypeStmt(typeStmt: TypeStmt): List[TypeIndex] = typeStmt match {
    case TypeStmt(name, items: TypeItems) =>
      items.items.map {
        case TypeItem(itemKey: TypeItemKey, TypeDef(itemType, isList)) =>
          TypeIndex(
            name.name,
            itemKey.value,
            itemType.name,
            DefinedType.parse(itemType.name),
            isList
          )
      }
  }

  /* ----- index conf statements and conf item expressions ----- */

  def indexConfStmts(stmts: List[Stmt]): List[ConfIndex] = stmts.flatMap {
    case confStmt: ConfStmt => indexConfStmt(confStmt)
    case _                  => None
  }

  def indexConfStmt(confStmt: ConfStmt): List[ConfIndex] = confStmt match {
    case ConfStmt(name, _, items) =>
      /** In order to index a confStmt, it needs to be converted to an object literal expression.
        *  Then it will be indexed as a top-level object in the index list, so it can be referenced
        *  inside other confStmts.
        */
      val confExpr = confStmtToExpr(confStmt)
      val ConfStmtIndexRow = ConfIndex(
        name.word,
        List.empty[String],
        confExpr,
        ObjectInferredType,
        hasReference(confExpr),
        isTopLevel = true
      )
      items.items
        .foldLeft(List(ConfStmtIndexRow)) {
          case (acc, item) =>
            indexConfItem(
              name = item.name.value,
              parents = List(name.word),
              expr = item.itemVal,
              index = acc
            )
        }
  }

  def indexConfItem(
      name: String,
      expr: Expr,
      parents: List[String] = List.empty[String],
      index: List[ConfIndex] = List.empty[ConfIndex]
  ): List[ConfIndex] = expr match {
    case w: LiteralWord =>
      index ::: ConfIndex(name, parents, w, WordInferredType, hasReference(w)) :: Nil
    case b: LiteralBool =>
      index ::: ConfIndex(name, parents, b, BoolInferredType, hasReference(b)) :: Nil
    case n: LiteralNumber =>
      index ::: ConfIndex(name, parents, n, NumberInferredType, hasReference(n)) :: Nil
    case s: LiteralString =>
      index ::: ConfIndex(name, parents, s, StringInferredType, hasReference(s)) :: Nil
    case a: LiteralArray =>
      index ::: ConfIndex(name, parents, a, ArrayInferredType, hasReference(a)) :: Nil
    case o @ LiteralObject(items: LiteralObjectItems) =>
      index ::: ConfIndex(name, parents, o, ObjectInferredType, hasReference(o)) :: indexObjectItems(
        name,
        items,
        parents
      )
    case p @ LiteralProto(_, items: LiteralObjectItems) =>
      index ::: ConfIndex(name, parents, p, ProtoInferredType, hasReference(p)) :: indexObjectItems(
        name,
        items,
        parents
      )
  }

  def indexObjectItems(
      name: String,
      objectItems: LiteralObjectItems,
      parents: List[String]
  ): List[ConfIndex] =
    objectItems.items.flatMap { item =>
      indexConfItem(name = item.name.value, expr = item.itemVal, parents = name :: parents)
    }
}
