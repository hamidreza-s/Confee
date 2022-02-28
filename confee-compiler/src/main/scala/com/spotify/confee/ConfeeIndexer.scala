package com.spotify.confee

import com.spotify.confee.ConfeeHelper.hasReference
import com.spotify.confee.Location.fromPosition

import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scala.util.{Failure, Success, Try}

object ConfeeIndexer {

  trait DefinedType {
    def name: String
    def isList: Boolean
    override def toString: String = if (isList) s"[$name]" else name
  }
  case object DefinedType {
    def parse(typeDef: TypeDef): DefinedType = typeDef match {
      case TypeDef(NameToken("Bool"), isList)   => BoolDefinedType("Bool", isList)
      case TypeDef(NameToken("Number"), isList) => NumberDefinedType("Number", isList)
      case TypeDef(NameToken("String"), isList) => StringDefinedType("String", isList)
      case TypeDef(NameToken(custom), isList)   => ObjectDefinedType(custom, isList)
    }
  }
  case class BoolDefinedType(name: String, isList: Boolean) extends DefinedType
  case class NumberDefinedType(name: String, isList: Boolean) extends DefinedType
  case class StringDefinedType(name: String, isList: Boolean) extends DefinedType
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
      stmt: TypeStmt,
      items: Map[String, DefinedType]
  )

  case class ConfIndex(
      name: String,
      expr: Expr,
      inferredType: InferredType,
      definedType: Option[DefinedType] = None,
      parents: List[String] = List.empty[String],
      isTopLevel: Boolean = false,
      hasReference: Boolean = false,
      isArrayItem: Boolean = false
  )

  case class Index(
      confIndex: ConfIndex,
      typeIndex: Either[ConfeeError, TypeIndex]
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
            throw ConfeeIndexerException(
              fromPosition(conf.expr.pos),
              s"Type error: '${conf.name}' conf does not have defined type"
            )
        }
      case Some(_) =>
        throw ConfeeIndexerException(
          fromPosition(conf.expr.pos),
          s"Type error: '${conf.name}' is not inside a valid top-level type"
        )
      case None =>
        val confParentsAscending = conf.parents.reverse
        confParentsAscending match {
          case Nil =>
            throw ConfeeIndexerException(
              fromPosition(conf.expr.pos),
              s"Type error: '${conf.name}' is top-level but has invalid structure"
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
              throw ConfeeIndexerException(
                fromPosition(conf.expr.pos),
                s"Type error: '${conf.name}' object item does not have defined type"
              )
          }
        case _ =>
          throw ConfeeIndexerException(
            fromPosition(conf.expr.pos),
            s"Type error: '${conf.name}' does not have a valid type structure"
          )
      }
  }

  def typeIndexLookupMaybe(
      conf: ConfIndex,
      typeIndex: List[TypeIndex],
      confIndex: List[ConfIndex]
  ): Either[ConfeeError, TypeIndex] = Try(typeIndexLookup(conf, typeIndex, confIndex)) match {
    case Success(typeIndex)                  => Right(typeIndex)
    case Failure(ex: ConfeeIndexerException) => Left(ConfeeIndexerError(ex.location, ex.msg))
    case Failure(ex)                         => Left(ConfeeUnknownError(ex))
  }

  /* ----- lookup conf ----- */

  def confIndexLookup(
      name: String,
      pos: Position,
      parents: List[String],
      index: List[ConfIndex]
  ): ConfIndex =
    index
      .foldLeft(List.empty[(Int, Int, ConfIndex)]) {
        case (acc, confIndex) =>
          val equalityHighPriority     = 0
          val equalityMediumPriority   = 1
          val proximityPriority        = parents.size - confIndex.parents.size
          val hasSameName              = confIndex.name.equals(name)
          val hasSameExactParents      = confIndex.parents.equals(parents)
          val hasSameUpperLevelParents = confIndex.parents.forall(parents.contains)

          (hasSameName, hasSameExactParents, hasSameUpperLevelParents) match {
            case (true, true, _) => (equalityHighPriority, proximityPriority, confIndex) :: acc
            case (true, _, true) => (equalityMediumPriority, proximityPriority, confIndex) :: acc
            case _               => acc
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
        throw ConfeeIndexerException(
          fromPosition(pos),
          s"Reference error: '$name' is not defined"
        )
    }

  def exprIndexExpansion[T <: Expr](
      name: String,
      pos: Position,
      parents: List[String],
      index: List[ConfIndex]
  ): T = {
    val indexRow = confIndexLookup(name, pos, parents, index)
    if (indexRow.hasReference) {
      throw ConfeeIndexerException(
        fromPosition(pos),
        s"Reference error: '$name' has a circular reference"
      )
    } else {
      indexRow.expr.asInstanceOf[T]
    }
  }

  def topLevelConfIndexLookup(
      name: String,
      confIndex: List[ConfIndex]
  ): ConfIndex =
    confIndex.find { conf =>
      val isTopLevel       = conf.isTopLevel
      val hasNoParents     = conf.parents.isEmpty
      val hasSameName      = conf.name.equals(name)
      val inferredAsObject = conf.inferredType.equals(ObjectInferredType)
      isTopLevel & hasNoParents & hasSameName & inferredAsObject
    } match {
      case Some(indexRow) => indexRow
      case None =>
        throw ConfeeException(s"Indexer error: '$name' is not a top-level conf")
    }

  def confStmtToExpr(confStmt: ConfStmt): Expr =
    LiteralObject(LiteralObjectItems(confStmt.items.items.map {
      case ConfItem(name, itemVal: LiteralExpr) =>
        LiteralObjectItem(LiteralObjectItemKey(name.value), itemVal)
    })).setPos(confStmt.pos)

  /* ----- index type and conf statements ----- */

  def indexStmts(stmts: List[Stmt]): List[Index] = {
    val typeIndex = indexTypeStmts(stmts)
    val confIndex = indexConfStmts(stmts)

    confIndex.map { confIndexItem =>
      Index(confIndexItem, typeIndexLookupMaybe(confIndexItem, typeIndex, confIndex))
    }
  }

  /* ----- index type statements ----- */

  def indexTypeStmts(stmts: List[Stmt]): List[TypeIndex] = stmts.flatMap {
    case typeStmt: TypeStmt => Some(indexTypeStmt(typeStmt))
    case _                  => None
  }

  def indexTypeStmt(typeStmt: TypeStmt): TypeIndex = typeStmt match {
    case TypeStmt(name, items: TypeItems) =>
      TypeIndex(name.name, typeStmt, items.items.map {
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
      index: List[ConfIndex] = List.empty[ConfIndex],
      isArrayItem: Boolean = false
  ): List[ConfIndex] = expr match {
    case w: LiteralWord =>
      index ::: ConfIndex(
        name = name,
        expr = w,
        inferredType = WordInferredType,
        parents = parents,
        hasReference = hasReference(w),
        isArrayItem = isArrayItem
      ) :: Nil
    case b: LiteralBool =>
      index ::: ConfIndex(
        name = name,
        expr = b,
        inferredType = BoolInferredType,
        parents = parents,
        hasReference = hasReference(b),
        isArrayItem = isArrayItem
      ) :: Nil
    case n: LiteralNumber =>
      index ::: ConfIndex(
        name = name,
        expr = n,
        inferredType = NumberInferredType,
        parents = parents,
        hasReference = hasReference(n),
        isArrayItem = isArrayItem
      ) :: Nil
    case s: LiteralString =>
      index ::: ConfIndex(
        name = name,
        expr = s,
        inferredType = StringInferredType,
        parents = parents,
        hasReference = hasReference(s),
        isArrayItem = isArrayItem
      ) :: Nil
    case a @ LiteralArray(items: List[LiteralExpr]) =>
      index ::: ConfIndex(
        name = name,
        expr = a,
        inferredType = ArrayInferredType,
        parents = parents,
        hasReference = hasReference(a),
        isArrayItem = isArrayItem
      ) :: indexArrayItems(
        name,
        items,
        parents
      )
    case o @ LiteralObject(items: LiteralObjectItems) =>
      index ::: ConfIndex(
        name = name,
        expr = o,
        inferredType = ObjectInferredType,
        parents = parents,
        hasReference = hasReference(o),
        isArrayItem = isArrayItem
      ) :: indexObjectItems(
        name,
        items,
        parents
      )
    case p @ LiteralProto(_, items: LiteralObjectItems) =>
      index ::: ConfIndex(
        name = name,
        expr = p,
        inferredType = ProtoInferredType,
        parents = parents,
        hasReference = hasReference(p),
        isArrayItem = isArrayItem
      ) :: indexObjectItems(
        name,
        items,
        parents
      )
  }

  def indexArrayItems(name: String, arrayItems: List[LiteralExpr], parents: List[String]): List[ConfIndex] =
    arrayItems.zipWithIndex.flatMap { case (item, i) =>
      indexConfItem(name = s"$name.$i", expr = item, parents = parents, isArrayItem = true)
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
