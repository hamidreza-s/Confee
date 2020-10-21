package com.spotify.confee

import org.scalatest.{FunSpec, Matchers}

class ConfeeEvaluatorTest extends FunSpec with Matchers {

  describe("Evaluator on literal string group") {
    it("should evaluate concat operator on literal string group") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "a" + "b"
                           |}""".stripMargin) shouldEqual Right(
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
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "a" - "b"
                           |     bat = "abc" - "b"
                           |     ban = "abcabc" - "abc"
                           |}""".stripMargin) shouldEqual Right(
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
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "a" + "bc" + "d"
                           |     bat = ("a" + "bc") + "d"
                           |     ban = "a" + ("bc" + "d")
                           |     bal = ("a" + "bc" + "d")
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(WordToken("bat"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(WordToken("ban"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(WordToken("bal"), LiteralStringFactor(StringToken("abcd")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate remove operator on literal string group in group (recursive)") {
      // NOTE: by default when there is no grouping string operators are right-associative
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "abc" - "efg" - "abc"
                           |     bat = "ab" - "bc" - "c"
                           |}""".stripMargin) shouldEqual Right(
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

    it("should fail when a literal word presents and has not been referenced in binder step") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "abc"
                           |     bat = "abc" + bar
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal String Word must have been referenced in binder step")
      )

      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "abc"
                           |     bat = bar + "abc"
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal String Word must have been referenced in binder step")
      )
    }

  }

  describe("Evaluator on literal number group") {
    it("should evaluate arithmetic operator on literal number group") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 10 + 2
                           |     bat = 10 - 2
                           |     ban = 10 * 2
                           |     bal = 10 / 2
                           |     baz = 10 % 2
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralNumberFactor(NumberToken(12))),
                  ConfItem(WordToken("bat"), LiteralNumberFactor(NumberToken(8))),
                  ConfItem(WordToken("ban"), LiteralNumberFactor(NumberToken(20))),
                  ConfItem(WordToken("bal"), LiteralNumberFactor(NumberToken(5))),
                  ConfItem(WordToken("baz"), LiteralNumberFactor(NumberToken(0)))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate arithmetic operator on literal number group in group (recursive)") {
      // NOTE: by default when there is no grouping arithmetic operators are right-associative
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 1 + (2 + 3) + 4
                           |     bat = 2 * (3 + 2)
                           |     ban = (4 - 2) * ((3 + 3) / (4 - 2)) + 5
                           |     bal = ((1 + 1) * (1 - 1)) + 1 + (5 % 2)
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralNumberFactor(NumberToken(10))),
                  ConfItem(WordToken("bat"), LiteralNumberFactor(NumberToken(10))),
                  ConfItem(WordToken("ban"), LiteralNumberFactor(NumberToken(16))),
                  ConfItem(WordToken("bal"), LiteralNumberFactor(NumberToken(2)))
                )
              )
            )
          )
        )
      )
    }

    it("should fail when a literal word presents and has not been referenced in binder step") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 123
                           |     bat = 123 + bar
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal Number Word must have been referenced in binder step")
      )

      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 123
                           |     bat = bar + 123
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal Number Word must have been referenced in binder step")
      )
    }
  }

  def assertEvaluatedAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens    <- ConfeeLexer(input).right
      parsed    <- ConfeeParser(tokens).right
      evaluated <- ConfeeEvaluator(parsed)
    } yield evaluated
  }
}
