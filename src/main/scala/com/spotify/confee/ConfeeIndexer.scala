package com.spotify.confee

import scala.util.parsing.input.Position

object ConfeeIndexer {
  case class IndexRow[T](
      name: WordToken,
      parents: List[WordToken] = List.empty[WordToken],
      expr: T,
      hasReference: Boolean
  )

  def indexLookup[T](
      name: WordToken,
      pos: Position,
      parents: List[WordToken],
      index: List[IndexRow[T]]
  ): T =
    index
      .foldLeft(List.empty[(Int, Int, IndexRow[T])]) {
        case (acc, indexRow) =>
          val equalityHighPriority     = 0
          val equalityLowPriority      = 1
          val proximityPriority        = parents.size - indexRow.parents.size
          val hasSameName              = indexRow.name.word.equals(name.word)
          val hasSameExactParents      = indexRow.parents.equals(parents)
          val hasSameUpperLevelParents = indexRow.parents.forall(parents.contains)

          (hasSameName, hasSameExactParents, hasSameUpperLevelParents) match {
            case (true, true, _) => (equalityHighPriority, proximityPriority, indexRow) :: acc
            case (true, _, true) => (equalityLowPriority, proximityPriority, indexRow) :: acc
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
      case Some(indexRow) =>
        if (indexRow.hasReference) {
          throw ConfeeException(
            Location(pos.line, pos.column),
            s"Reference error: '${name.word}' has a circular reference"
          )
        } else {
          indexRow.expr
        }
      case None =>
        throw ConfeeException(
          Location(pos.line, pos.column),
          s"Reference error: '${name.word}' is not defined"
        )
    }
}
