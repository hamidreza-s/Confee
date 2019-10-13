package com.spotify.confee

import scala.util.parsing.input.Positional

sealed trait ConfeeAST extends Positional

case class Stmt(name: String, items: List[Any]) extends ConfeeAST
