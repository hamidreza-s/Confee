package com.spotify.confee

import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}

class ConfeeLexerTest extends FunSpec with Matchers with BeforeAndAfterEach {

  describe("Lexer on strings") {

    it("should tokenize small strings") {
      assertTokens(
        """ "abc" "abc" "abc" """,
        List(
          StringToken("abc"),
          StringToken("abc"),
          StringToken("abc")
        )
      )
    }

    it("should tokenize strings with uppercase, lowercase, and number") {
      assertTokens(
        """ "ABCabc123" "abcABC123" "123abcABC" "123ABCabc" """,
        List(
          StringToken("ABCabc123"),
          StringToken("abcABC123"),
          StringToken("123abcABC"),
          StringToken("123ABCabc")
        )
      )
    }

    it("should tokenize large strings") {
      val largeString = (1 to 4).foldLeft("") { case (s: String, _: Int) => s + "ABCabc123" }
      assertTokens(
        s""" "$largeString" "$largeString" """,
        List(
          StringToken(largeString),
          StringToken(largeString)
        )
      )
    }

    it("should tokenize strings with symbols") {
      assertTokens(
        """ "ABC123[]^_`~/.;ABC123" "ABC123!@#$%^&*()_+ABC123" """,
        List(
          StringToken("ABC123[]^_`~/.;ABC123"),
          StringToken("ABC123!@#$%^&*()_+ABC123")
        )
      )
    }

    it("should tokenize strings with escape character") {
      assertTokens(
        """ "abc\"abc" "abc\\abc" """,
        List(
          StringToken("abc\"abc"),
          StringToken("abc\\abc")
        )
      )
    }

    it("should tokenize strings with unicode") {
      assertTokens(
        """ "我不知道中文是什么意思" "Täby" "حمیدرضا" """,
        List(
          StringToken("我不知道中文是什么意思"),
          StringToken("Täby"),
          StringToken("حمیدرضا")
        )
      )
    }
  }

  describe("Lexer on numbers") {

    it("should tokenize small positive numbers") {
      assertTokens(
        "1 +2 3 +4",
        List(
          NumberToken(1.0),
          NumberToken(2.0),
          NumberToken(3.0),
          NumberToken(4.0)
        )
      )
    }

    it("should tokenize small negative numbers") {
      assertTokens(
        "-1 -2 -3 -4",
        List(
          NumberToken(-1.0),
          NumberToken(-2.0),
          NumberToken(-3.0),
          NumberToken(-4.0)
        )
      )
    }

    it("should tokenize long positive numbers") {
      assertTokens(
        "+111111111111111111111111 222222222222222222222222",
        List(
          NumberToken(111111111111111111111111.0),
          NumberToken(222222222222222222222222.0)
        )
      )
    }

    it("should tokenize long negative numbers") {
      assertTokens(
        "-111111111111111111111111 -222222222222222222222222",
        List(
          NumberToken(-111111111111111111111111.0),
          NumberToken(-222222222222222222222222.0)
        )
      )
    }

    it("should tokenize longest positive number") {
      assertTokens(
        BigDecimal(Double.MaxValue).toBigInt.toString,
        List(
          NumberToken(1.7976931348623157E308)
        )
      )
    }

    it("should tokenize longest negative number") {
      assertTokens(
        BigDecimal(Double.MinValue).toBigInt.toString,
        List(
          NumberToken(-1.7976931348623157E308)
        )
      )
    }

    it("should tokenize numbers with extra spaces and new lines") {
      assertTokens(
        """
          |1234
          |1234  -1234
          |1234         -1234
        """.stripMargin,
        List(
          NumberToken(1234),
          NumberToken(1234),
          NumberToken(-1234),
          NumberToken(1234),
          NumberToken(-1234)
        )
      )
    }

    it("should tokenize numbers with floating points") {
      assertTokens(
        "1234.5678 -1234.5678",
        List(
          NumberToken(1234.5678),
          NumberToken(-1234.5678)
        )
      )
    }
  }

  describe("Lexer on keywords") {

    it("should tokenize keywords") {
      assertTokens("type conf",
        List(
          TypeKeywordToken(),
          ConfKeywordToken()
        )
      )
    }
  }

  describe("Lexer on identifiers") {

    it("should tokenize names") {
      assertTokens(
        "NamesStartWithUpperCase Foo Bar",
        List(
          NameToken("NamesStartWithUpperCase"),
          NameToken("Foo"),
          NameToken("Bar"),
        )
      )
    }

    it("should tokenize words") {
      assertTokens(
        "wordsStartWithLowerCase foo bar",
        List(
          WordToken("wordsStartWithLowerCase"),
          WordToken("foo"),
          WordToken("bar")
        )
      )
    }
  }

  describe("Lexer on operators") {

    it("should tokenize arithmetic and assignment operators") {
      assertTokens(
        "+ - / * % =",
        List(
          AdditionToken(),
          SubtractionToken(),
          DivisionToken(),
          MultiplicationToken(),
          ModulusToken(),
          AssignmentToken()
        )
      )
    }

    it("should tokenize delimiter and grouping operators") {
      assertTokens(
        "( ) [ ] { } , : ; # .",
        List(
          ParenthesesOpenToken(),
          ParenthesesCloseToken(),
          BracketOpenToken(),
          BracketCloseToken(),
          BraceOpenToken(),
          BraceCloseToken(),
          SeparatorToken(),
          ColonToken(),
          SemiColonToken(),
          HashToken(),
          DotToken()
        )
      )
    }
  }

  describe("Lexer on all") {

    it("should tokenize strings, numbers, identifiers, and operators") {
      assertTokens(
        """
          |"abc" "ABC" "abc123ABC!@#"
          |123.456 -123.456 123456 -123456
          |type conf
          |NamesStartWithUpperCase Foo Bar
          |wordsStartWithLowerCase foo bar
          |+ - / * % =
          |( ) [ ] { } , : ; # .
        """.stripMargin,
        List(
          StringToken("abc"), StringToken("ABC"), StringToken("abc123ABC!@#"),
          NumberToken(123.456), NumberToken(-123.456), NumberToken(123456), NumberToken(-123456),
          TypeKeywordToken(), ConfKeywordToken(),
          NameToken("NamesStartWithUpperCase"), NameToken("Foo"), NameToken("Bar"),
          WordToken("wordsStartWithLowerCase"), WordToken("foo"), WordToken("bar"),
          AdditionToken(),
          SubtractionToken(),
          DivisionToken(),
          MultiplicationToken(),
          ModulusToken(),
          AssignmentToken(),
          ParenthesesOpenToken(),
          ParenthesesCloseToken(),
          BracketOpenToken(),
          BracketCloseToken(),
          BraceOpenToken(),
          BraceCloseToken(),
          SeparatorToken(),
          ColonToken(),
          SemiColonToken(),
          HashToken(),
          DotToken()
        )
      )
    }
  }

  def assertTokens(input: String, expectedOutput: List[ConfeeToken]): Unit = {
    ConfeeLexer(input) match {
      case Right(tokens) =>
        tokens shouldEqual expectedOutput
      case Left(error) => fail(error.toString)
    }
  }

}
