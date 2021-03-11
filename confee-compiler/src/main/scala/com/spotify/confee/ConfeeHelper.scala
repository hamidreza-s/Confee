package com.spotify.confee

object ConfeeHelper {
  def hasReference(expr: Expr): Boolean = expr match {
    // literal word expressions
    case _: LiteralWord => true
    // literal bool expressions
    case _: LiteralBoolFactor => false
    case _: LiteralBoolWord   => true
    case e: LiteralBoolUnit   => hasReference(e.unit)
    case e: LiteralBoolGroup  => hasReference(e.left) || hasReference(e.right)
    // literal string expressions
    case _: LiteralStringFactor => false
    case _: LiteralStringWord   => true
    case e: LiteralStringGroup  => hasReference(e.left) || hasReference(e.right)
    // literal number expressions
    case _: LiteralNumberFactor => false
    case _: LiteralNumberWord   => true
    case e: LiteralNumberGroup  => hasReference(e.left) || hasReference(e.right)
    // literal Array expressions
    case e: LiteralArray => e.items.exists(hasReference)
    // literal object expressions
    case e: LiteralObject => e.items.items.exists(i => hasReference(i.itemVal))
    // literal proto expressions
    case e: LiteralProto => e.items.items.exists(i => hasReference(i.itemVal))
  }
}
