package com.spotify.confee

import com.spotify.confee.ConfeeHelper.hasReference

import scala.annotation.tailrec
import scala.util.parsing.input.{NoPosition, Position}

object ConfeeIndexer {

  // DONE (1): Add type information of expressions defined in type statement when indexing and use it when looking up
  // TODO (2): LiteralExpr/AnyType should be replaced by the type of expression type it is referring to, because it might be a Bool, Number, String, Array, or Object
  // TODO (3): When (1) and (2) are done, set the `hasSameType` condition to be true for all cases

  trait DefinedType {
    def isList: Boolean
  }
  case object DefinedType {
    def parse(typeDef: TypeDef): DefinedType = typeDef match {
      case TypeDef(NameToken("Bool"), isList)   => BoolDefinedType(isList)
      case TypeDef(NameToken("Number"), isList) => NumberDefinedType(isList)
      case TypeDef(NameToken("String"), isList) => StringDefinedType(isList)
      case TypeDef(NameToken(custom), isList)   => ObjectDefinedType(custom, isList)
    }
  }
  case class BoolDefinedType(isList: Boolean) extends DefinedType
  case class NumberDefinedType(isList: Boolean) extends DefinedType
  case class StringDefinedType(isList: Boolean) extends DefinedType
  case class ObjectDefinedType(name: String, isList: Boolean) extends DefinedType

  sealed trait InferredType
  case object WordInferredType extends InferredType
  case object BoolInferredType extends InferredType
  case object NumberInferredType extends InferredType
  case object StringInferredType extends InferredType
  case object ArrayInferredType extends InferredType
  case object ObjectInferredType extends InferredType
  case object ProtoInferredType extends InferredType
  case object NoInferredType extends InferredType

  case class TypeIndex(
      name: String,
      items: Map[String, DefinedType]
  )

  case class ConfIndex(
      name: String,
      parents: List[String] = List.empty[String],
      expr: Expr,
      inferredType: InferredType, // TODO: move it next to definedType
      hasReference: Boolean,
      isTopLevel: Boolean = false,
      definedType: Option[DefinedType] = None
  )

  case class Index(
      confIndex: ConfIndex,
      typeIndex: TypeIndex
  )

  // TODO: make sure it won't go into infinite loop
  // TODO: just pass confIndex's name instead of confIndexItem
  def typeIndexLookup(
      confIndexItem: ConfIndex,
      typeIndex: List[TypeIndex],
      confIndex: List[ConfIndex]
  ): TypeIndex =
    confIndexItem.definedType match {
      case Some(ObjectDefinedType(typeName, _)) =>
        typeIndex.find { typeIndexRow =>
          typeIndexRow.name.equals(typeName)
        } match {
          case Some(indexRow) => indexRow
          case None =>
            throw ConfeeException(
              Location(confIndexItem.expr.pos.line, confIndexItem.expr.pos.column),
              s"Indexer error: '${confIndexItem.name}' does not have defined type"
            )
        }
      case Some(_) =>
        throw ConfeeException(
          Location(confIndexItem.expr.pos.line, confIndexItem.expr.pos.column),
          s"Indexer error: '${confIndexItem.name}' is not inside a valid top-level type"
        )
      case None =>
        val confParentsAscending = confIndexItem.parents.reverse
        confParentsAscending match {
          case Nil =>
            throw ConfeeException(
              Location(confIndexItem.expr.pos.line, confIndexItem.expr.pos.column),
              s"Indexer error: '${confIndexItem.name}' is top-level but has invalid structure"
            )
          case topLevelParent :: tailParent =>
            val topLevelParentConfIndex = confIndexLookup(topLevelParent, confIndex)
            val topLevelParentTypeIndex =
              typeIndexLookup(topLevelParentConfIndex, typeIndex, confIndex)
            typeIndexLookupNested(
              topLevelParentTypeIndex,
              tailParent,
              typeIndex,
              confIndex
            )
        }
    }

  @tailrec
  def typeIndexLookupNested(
      currentTypeIndex: TypeIndex,
      pathToItem: List[String],
      typeIndex: List[TypeIndex],
      confIndex: List[ConfIndex]
  ): TypeIndex = pathToItem match {
    case Nil => currentTypeIndex
    case head :: tail =>
      currentTypeIndex.items.get(head) match {
        case Some(ObjectDefinedType(nextTypeName, _)) =>
          typeIndex.find { typeIndexRow =>
            typeIndexRow.name.equals(nextTypeName)
          } match {
            case Some(indexRow) => typeIndexLookupNested(indexRow, tail, typeIndex, confIndex)
            case None =>
              throw ConfeeException(Location(0, 0), "Implement me!") // TODO: Implement it!
          }
      }
  }

  def confIndexLookup(
      name: String,
      index: List[ConfIndex]
  ): ConfIndex =
    confIndexLookupWithItems(name, ObjectInferredType, NoPosition, List.empty[String], index)

  def confIndexLookupWithItems(
      name: String,
      exprType: InferredType,
      pos: Position,
      parents: List[String],
      index: List[ConfIndex]
  ): ConfIndex =
    index
      .foldLeft(List.empty[(Int, Int, ConfIndex)]) {
        case (acc, indexRow) =>
          val equalityHighPriority     = 0
          val equalityMediumPriority   = 1
          val equalityLowPriority      = 2
          val proximityPriority        = parents.size - indexRow.parents.size
          val hasSameName              = indexRow.name.equals(name)
          val hasSameType              = indexRow.inferredType.equals(exprType)
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
      case Some(indexRow) => indexRow
      case None =>
        throw ConfeeException(
          Location(pos.line, pos.column),
          s"Reference error: '$name' is not defined"
        )
    }

  def exprIndexExpansion[T <: Expr](
      name: String,
      exprType: InferredType,
      pos: Position,
      parents: List[String],
      index: List[ConfIndex]
  ): T = {
    val indexRow = confIndexLookupWithItems(name, exprType, pos, parents, index)
    if (indexRow.hasReference) {
      throw ConfeeException(
        Location(pos.line, pos.column),
        s"Reference error: '$name' has a circular reference"
      )
    } else {
      indexRow.expr.asInstanceOf[T]
    }
  }

  def confStmtToExpr(confStmt: ConfStmt): Expr =
    LiteralObject(LiteralObjectItems(confStmt.items.items.map {
      case ConfItem(name, itemVal: LiteralExpr) =>
        LiteralObjectItem(LiteralObjectItemKey(name.value), itemVal)
    }))

  /* ----- index type and conf statements ----- */

  def index(stmts: List[Stmt]): List[Index] = {
    val typeIndex = indexTypeStmts(stmts)
    val confIndex = indexConfStmts(stmts)

    confIndex.map { confIndexItem =>
      Index(confIndexItem, typeIndexLookup(confIndexItem, typeIndex, confIndex))
    }
  }

  /* ----- index type statements ----- */

  def indexTypeStmts(stmts: List[Stmt]): List[TypeIndex] = stmts.flatMap {
    case typeStmt: TypeStmt => Some(indexTypeStmt(typeStmt))
    case _                  => None
  }

  def indexTypeStmt(typeStmt: TypeStmt): TypeIndex = typeStmt match {
    case TypeStmt(name, items: TypeItems) =>
      TypeIndex(name.name, items.items.map {
        case TypeItem(itemKey: TypeItemKey, typeDef: TypeDef) =>
          itemKey.value -> DefinedType.parse(typeDef)
      }.toMap)
  }

  /* ----- index conf statements and its item expressions ----- */

  def indexConfStmts(stmts: List[Stmt]): List[ConfIndex] = stmts.flatMap {
    case confStmt: ConfStmt => indexConfStmt(confStmt)
    case _                  => None
  }

  def indexConfStmt(confStmt: ConfStmt): List[ConfIndex] = confStmt match {
    case ConfStmt(name, typeDef, items) =>
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
        isTopLevel = true,
        Some(DefinedType.parse(typeDef))
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
