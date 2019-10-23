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

  def char: Parser[Char] = ("""[^"\\]""".r | '\\' ~> ".".r) ^^ { _.head }

  def string: Parser[ConfeeToken] = "\"" ~> rep(char) <~ "\"" ^^ { chars => StringToken(chars.mkString) }

  def number: Parser[ConfeeToken] = """(\-|\+)?\d+(\.\d+)?""".r ^^ { s => NumberToken(s.toDouble) }

  def typeKeyword: Parser[ConfeeToken] = "type" ^^ { _ => TypeKeywordToken() }

  def factKeyword: Parser[ConfeeToken] = "fact" ^^ { _ => FactKeywordToken() }

  def word: Parser[ConfeeToken] = """[a-z][a-zA-Z0-9_]*""".r ^^ { s => WordToken(s) }

  def name: Parser[ConfeeToken] = """[A-Z][a-zA-Z0-9_]*""".r ^^ { s => NameToken(s) }

  def addition: Parser[ConfeeToken] = """\+""".r ^^ { _ => AdditionToken() }

  def subtraction: Parser[ConfeeToken] = """\-""".r ^^ { _ => SubtractionToken() }

  def division: Parser[ConfeeToken] = """\/""".r ^^ { _ => DivisionToken() }

  def multiplication: Parser[ConfeeToken] = """\*""".r ^^ { _ => MultiplicationToken() }

  def modulus: Parser[ConfeeToken] = """\%""".r ^^ { _ => ModulusToken() }

  def assignment: Parser[ConfeeToken] = """\=""".r ^^ { _ => AssignmentToken() }

  def parenthesesOpen: Parser[ConfeeToken] = """\(""".r ^^ { _ => ParenthesesOpenToken() }

  def parenthesesClose: Parser[ConfeeToken] = """\)""".r ^^ { _ => ParenthesesCloseToken() }

  def bracketOpen: Parser[ConfeeToken] = """\[""".r ^^ { _ => BracketOpenToken() }

  def bracketClose: Parser[ConfeeToken] = """\]""".r ^^ { _ => BracketCloseToken() }

  def braceOpen: Parser[ConfeeToken] = """\{""".r ^^ { _ => BraceOpenToken() }

  def braceClose: Parser[ConfeeToken] = """\}""".r ^^ { _ => BraceCloseToken() }

  def separator: Parser[ConfeeToken] = """,""".r ^^ { _ => SeparatorToken() }

  def colon: Parser[ConfeeToken] = """:""".r ^^ { _ => ColonToken() }

  def semiColon: Parser[ConfeeToken] = """;""".r ^^ { _ => SemiColonToken() }

  def hash: Parser[ConfeeToken] = """#""".r ^^ { _ => HashToken() }

  def dot: Parser[ConfeeToken] = """\.""".r ^^ { _ => DotToken() }

  def singleComment: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ Unit

  def multiComment: Parser[Unit] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ Unit

  def comment: Parser[Unit] = singleComment | multiComment

  def skip: Parser[Unit] = rep(whiteSpace | comment) ^^^ Unit

  def token: Parser[ConfeeToken] = positioned {
    string | number | typeKeyword | factKeyword | word | name |
    addition | subtraction | division | multiplication | modulus | assignment |
    parenthesesOpen | parenthesesClose | bracketOpen | bracketClose | braceOpen | braceClose |
    separator | colon | semiColon | hash | dot
  }

  def tokens: Parser[List[ConfeeToken]] = skip ~> rep(token <~ skip) <~ skip
}
