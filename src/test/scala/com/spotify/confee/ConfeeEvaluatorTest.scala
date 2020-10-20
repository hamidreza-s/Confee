package com.spotify.confee

import org.scalatest.{FunSpec, Matchers}

class ConfeeEvaluatorTest extends FunSpec with Matchers {

  describe("Evaluator on literal string group") {
    it("should evaluate concat operator on literal string group") {
      assertEvaluatedAST(
        """conf foo : Foo {
          |     bar = "a" + "b"
          |}""".stripMargin,
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("ab")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate remove operator on literal string group") {
      assertEvaluatedAST(
        """conf foo : Foo {
          |     bar = "a" - "b"
          |     bat = "abc" - "b"
          |     ban = "abcabc" - "abc"
          |}""".stripMargin,
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("a"))),
                  ConfItem(WordToken("bat"), LiteralStringFactor(StringToken("ac"))),
                  ConfItem(WordToken("ban"), LiteralStringFactor(StringToken("abc")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate concat operator on literal string group in group (recursive)") {
      assertEvaluatedAST(
        """conf foo : Foo {
          |     bar = "a" + "bc" + "d"
          |}""".stripMargin,
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("abcd")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate remove operator on literal string group in group (recursive)") {
      // NOTE: operators are right-associative in confee
      assertEvaluatedAST(
        """conf foo : Foo {
          |     bar = "abc" - "efg" - "abc"
          |     bat = "ab" - "bc" - "c"
          |}""".stripMargin,
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("abc"))),
                  ConfItem(WordToken("bat"), LiteralStringFactor(StringToken("a")))
                )
              )
            )
          )
        )
      )
    }
  }

  describe("Evaluator on literal number group") {
    it("should evaluate ...") {}
  }

  describe("Evaluator on variable binding") {
    it("should evaluate ...") {}
  }

  def assertEvaluatedAST(input: String, expectedOutput: ConfeeAST): Unit = {
    (for {
      tokens    <- ConfeeLexer(input).right
      parsed    <- ConfeeParser(tokens).right
      evaluated <- ConfeeEvaluator(parsed)
    } yield evaluated) match {
      case Right(ast)  => ast shouldEqual expectedOutput
      case Left(error) => fail(error.toString)
    }
  }
}
