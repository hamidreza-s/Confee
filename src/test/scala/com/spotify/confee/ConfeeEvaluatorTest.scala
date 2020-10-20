package com.spotify.confee

import org.scalatest.{FunSpec, Matchers}

class ConfeeEvaluatorTest extends FunSpec with Matchers {

  describe("Evaluator on literal string group") {
    it("should evaluate ...") {
      assertEvaluatedAST(
        """conf foo : Foo {
          |     i1 = "ab" + "c"
          |     i2 = "abcd" - "d"
          |     i3 = {
          |          j1 = "a" + "b" + "c"
          |     }
          |}""".stripMargin,
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("i1"), LiteralStringFactor(StringToken("abc"))),
                  ConfItem(WordToken("i2"), LiteralStringFactor(StringToken("abc"))),
                  ConfItem(
                    WordToken("i3"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("j1"),
                            LiteralStringFactor(StringToken("abc"))
                          )
                        )
                      )
                    )
                  )
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
