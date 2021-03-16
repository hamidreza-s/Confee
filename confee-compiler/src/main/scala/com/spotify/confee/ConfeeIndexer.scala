package com.spotify.confee

import com.spotify.confee.ConfeeHelper.hasReference

import scala.annotation.tailrec
import scala.util.parsing.input.{NoPosition, Position}

object ConfeeIndexer {

  // TODO:
  //  1. Add type information of expressions defined in type statement when indexing and use it when looking up [done]
  //  3. Set the `hasSameType` condition to be true for all cases

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
      expr: Expr,
      inferredType: InferredType,
      definedType: Option[DefinedType] = None,
      parents: List[String] = List.empty[String],
      isTopLevel: Boolean = false,
      hasReference: Boolean = false
  )

  case class Index(
      confIndex: ConfIndex,
      typeIndex: TypeIndex
  )

  /* ----- lookup type ----- */

  def typeIndexLookup(
      conf: ConfIndex,
      typeIndex: List[TypeIndex],
      confIndex: List[ConfIndex]
  ): TypeIndex =
    conf.definedType match {
      case Some(ObjectDefinedType(typeName, _)) =>
        typeIndex.find { typeIndexRow =>
          typeIndexRow.name.equals(typeName)
        } match {
          case Some(indexRow) => indexRow
          case None =>
            throw ConfeeException(
              Location(conf.expr.pos.line, conf.expr.pos.column),
              s"Indexer error: '${conf.name}' conf does not have defined type"
            )
        }
      case Some(_) =>
        throw ConfeeException(
          Location(conf.expr.pos.line, conf.expr.pos.column),
          s"Indexer error: '${conf.name}' is not inside a valid top-level type"
        )
      case None =>
        val confParentsAscending = conf.parents.reverse
        confParentsAscending match {
          case Nil =>
            throw ConfeeException(
              Location(conf.expr.pos.line, conf.expr.pos.column),
              s"Indexer error: '${conf.name}' is top-level but has invalid structure"
            )
          case topLevelParent :: tailParent =>
            val topLevelParentConf = topLevelConfIndexLookup(topLevelParent, confIndex)
            val topLevelParentType = typeIndexLookup(topLevelParentConf, typeIndex, confIndex)

            /**
              * The typeIndexLookup method gets called when we are looking for a type of
              * an object item which in any level lower than a conf. Since objects can be nested,
              * so we need to do a recursive call to reach to the type we are looking for.
              */
            typeIndexLookupLoop(conf, topLevelParentType, tailParent, typeIndex, confIndex)
        }
    }

  @tailrec
  private def typeIndexLookupLoop(
      conf: ConfIndex,
      currentType: TypeIndex,
      pathToItem: List[String],
      typeIndex: List[TypeIndex],
      confIndex: List[ConfIndex]
  ): TypeIndex = pathToItem match {
    case Nil => currentType
    case head :: tail =>
      currentType.items.get(head) match {
        case Some(ObjectDefinedType(nextTypeName, _)) =>
          typeIndex.find { typeIndexRow =>
            typeIndexRow.name.equals(nextTypeName)
          } match {
            case Some(indexRow) =>
              typeIndexLookupLoop(conf, indexRow, tail, typeIndex, confIndex)
            case None =>
              throw ConfeeException(
                Location(conf.expr.pos.line, conf.expr.pos.column),
                s"Indexer error: '${conf.name}' object item does not have defined type"
              )
          }
        case _ =>
          throw ConfeeException(
            Location(conf.expr.pos.line, conf.expr.pos.column),
            s"Indexer error: '${conf.name}' does not have a valid type structure"
          )
      }
  }

  /* ----- lookup conf ----- */

  def confIndexLookup(
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
    val indexRow = confIndexLookup(name, exprType, pos, parents, index)
    if (indexRow.hasReference) {
      throw ConfeeException(
        Location(pos.line, pos.column),
        s"Reference error: '$name' has a circular reference"
      )
    } else {
      indexRow.expr.asInstanceOf[T]
    }
  }

  def topLevelConfIndexLookup(
      name: String,
      index: List[ConfIndex]
  ): ConfIndex =
    confIndexLookup(name, ObjectInferredType, NoPosition, List.empty[String], index)

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
      /**
        * In order to index a confStmt, it needs to be converted to an object literal expression.
        * Then it will be indexed as a top-level object in the index list, so it can be referenced
        * inside other confStmts.
        */
      val confExpr = confStmtToExpr(confStmt)
      val ConfStmtIndexRow = ConfIndex(
        name.word,
        confExpr,
        ObjectInferredType,
        Some(DefinedType.parse(typeDef)),
        List.empty[String],
        isTopLevel = true,
        hasReference(confExpr)
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
      index ::: ConfIndex(
        name,
        expr = w,
        inferredType = WordInferredType,
        parents = parents,
        hasReference = hasReference(w)
      ) :: Nil
    case b: LiteralBool =>
      index ::: ConfIndex(
        name,
        expr = b,
        inferredType = BoolInferredType,
        parents = parents,
        hasReference = hasReference(b)
      ) :: Nil
    case n: LiteralNumber =>
      index ::: ConfIndex(
        name,
        expr = n,
        inferredType = NumberInferredType,
        parents = parents,
        hasReference = hasReference(n)
      ) :: Nil
    case s: LiteralString =>
      index ::: ConfIndex(
        name,
        expr = s,
        inferredType = StringInferredType,
        parents = parents,
        hasReference = hasReference(s)
      ) :: Nil
    case a: LiteralArray =>
      index ::: ConfIndex(
        name,
        expr = a,
        inferredType = ArrayInferredType,
        parents = parents,
        hasReference = hasReference(a)
      ) :: Nil
    case o @ LiteralObject(items: LiteralObjectItems) =>
      index ::: ConfIndex(
        name,
        expr = o,
        inferredType = ObjectInferredType,
        parents = parents,
        hasReference = hasReference(o)
      ) :: indexObjectItems(
        name,
        items,
        parents
      )
    case p @ LiteralProto(_, items: LiteralObjectItems) =>
      index ::: ConfIndex(
        name,
        expr = p,
        inferredType = ProtoInferredType,
        parents = parents,
        hasReference = hasReference(p)
      ) :: indexObjectItems(
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
