package com.spotify.confee

import scala.collection.immutable.HashSet
import scala.util.parsing.combinator._

object ConfeeLexer extends RegexParsers {

  def apply(code: String): Either[ConfeeLexerError, List[ConfeeToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) =>
        Left(ConfeeLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) =>
        Right(result)
    }
  }

  override type Elem = Char

  override def skipWhitespace: Boolean = false

  val keywords = HashSet("fact", "func", "type")

  def char: Parser[Char] = ("""[^"\\]""".r | '\\' ~> ".".r) ^^ { _.head }

  def string: Parser[ConfeeToken] = "\"" ~> rep(char) <~ "\"" ^^ { chars => StringConfeeToken(chars.mkString) }

  def number: Parser[ConfeeToken] = """\d+(\.\d+)?""".r ^^ (s => NumberConfeeToken(s.toDouble))

  def operator: Parser[ConfeeToken] = """->|==|!=|\\|\+|-|/|\*|=|:|::|\{|\}|,""".r ^^ OperatorConfeeToken

  def punctuation: Parser[ConfeeToken] = """\(|\)""".r ^^ PunctuationConfeeToken

  def name: Parser[ConfeeToken] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ {
    case s if keywords.contains(s) => KeywordConfeeToken(s)
    case s => NameConfeeToken(s)
  }

  def singleComment: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ Unit

  def multiComment: Parser[Unit] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ Unit

  def comment: Parser[Unit] = singleComment | multiComment

  def skip: Parser[Unit] = rep(whiteSpace | comment) ^^^ Unit

  def token: Parser[ConfeeToken] = positioned(operator | punctuation | name | number | string)

  def tokens: Parser[List[ConfeeToken]] = skip ~> rep(token <~ skip) <~ skip
}
