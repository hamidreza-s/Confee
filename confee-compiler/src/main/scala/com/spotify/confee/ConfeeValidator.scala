package com.spotify.confee

import com.spotify.confee.Location.fromPosition

object ConfeeValidator {
  def apply(ast: ConfeeAST, skip: Boolean = false): Either[ConfeeError, ConfeeAST] = {
    if (skip) Right(ast)
    else {
      ast match {
        case Grammar(stmts: List[Stmt]) =>
          val duplicatedConfs     = findDuplicatedConfName(stmts)
          val duplicateConfItems  = findDuplicateConfItemNames(stmts)
          val duplicatedTypes     = findDuplicatedTypeName(stmts)
          val duplicatedTypeItems = findDuplicatedTypeItemNames(stmts)

          if (duplicatedConfs.isEmpty
                & duplicateConfItems.isEmpty
                & duplicatedTypes.isEmpty
                & duplicatedTypeItems.isEmpty) {
            Right(ast)
          } else {
            val confErrors = duplicatedConfs.map { c =>
              ConfeeValidatorError(
                fromPosition(c.name.pos),
                s"Validator error: duplicate conf name: ${c.name}"
              )
            }

            val confItemErrors = duplicateConfItems.map { confItem =>
              ConfeeValidatorError(
                fromPosition(confItem.itemVal.pos),
                s"Validation error: duplicated item name of conf: ${confItem.name}"
              )
            }

            val typeErrors = duplicatedTypes.map { t =>
              ConfeeValidatorError(
                fromPosition(t.name.pos),
                s"Validator error: duplicated type name: ${t.name}"
              )
            }

            val typeItemErrors = duplicatedTypeItems.map { typeItem =>
              ConfeeValidatorError(
                fromPosition(typeItem.itemType.name.pos),
                s"Validator error: duplicated item name of type: ${typeItem.name}"
              )
            }

            val sortedDistinctErrors =
              (confErrors ::: confItemErrors ::: typeErrors ::: typeItemErrors).sortBy { e =>
                (e.location.line, e.location.column)
              }.distinct

            Left(ConfeeValidatorErrors(sortedDistinctErrors))
          }
        case otherwise =>
          Left(
            ConfeeConstructorError(
              Location(otherwise.pos.line, otherwise.pos.column),
              "AST in validator step does not contain valid grammar structure"
            )
          )
      }
    }
  }

  private def findDuplicatedConfName(stmts: List[Stmt]): List[ConfStmt] =
    stmts
      .flatMap {
        case confStmt: ConfStmt => Some(confStmt)
        case _                  => None
      }
      .groupBy(_.name)
      .flatMap {
        case (_, grouped: Iterable[ConfStmt]) =>
          if (grouped.size > 1) grouped else None

      }
      .toList

  private def findDuplicateConfItemNames(stmts: List[Stmt]) =
    stmts
      .flatMap {
        case ConfStmt(_, _, confItems) =>
          confItems.items
            .groupBy(_.name)
            .flatMap {
              case (_, grouped: Iterable[ConfItem]) =>
                if (grouped.size > 1) grouped else None
            }
            .toList
        case _ => None
      }

  private def findDuplicatedTypeName(stmts: List[Stmt]): List[TypeStmt] =
    stmts
      .flatMap {
        case typeStmt: TypeStmt => Some(typeStmt)
        case _                  => None
      }
      .groupBy(_.name)
      .flatMap {
        case (_, grouped: Iterable[TypeStmt]) =>
          if (grouped.size > 1) grouped else None
      }
      .toList

  private def findDuplicatedTypeItemNames(stmts: List[Stmt]): List[TypeItem] =
    stmts
      .flatMap {
        case TypeStmt(_, typeItems) =>
          typeItems.items
            .groupBy(_.name)
            .flatMap {
              case (_, grouped: Iterable[TypeItem]) =>
                if (grouped.size > 1) grouped else None
            }
            .toList
        case _ => None
      }
}
