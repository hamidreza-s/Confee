package com.spotify.confee

import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}

class ConfeeParserTest extends FunSpec with Matchers with BeforeAndAfterEach {

  describe("Parser on type statement") {

    it("should parse type definitions without type items") {
      assertAST(
        """
          |type Foo { }
          |type Bar { }
          |""".stripMargin,
        Grammar(List(
          TypeStmt(NameToken("Foo"), List()),
          TypeStmt(NameToken("Bar"), List())
        ))
      )
    }
  }

  describe("Parser on fact statement") {

    it("should parse fact definitions without fact items") {
      assertAST(
        """
          |fact foo : Foo { }
          |fact bar : foo { }
          |""".stripMargin,
        Grammar(List(
          FactStmt(WordToken("foo"), TypeDef(Left(NameToken("Foo")), isList = false), List()),
          FactStmt(WordToken("bar"), TypeDef(Right(WordToken("foo")), isList = false), List())
        ))
      )
    }
  }

  def assertAST(input: String, expectedOutput: ConfeeAST): Unit = {
    (for {
      tokens <- ConfeeLexer(input).right
      ast <- ConfeeParser(tokens)
    } yield ast) match {
      case Right(ast) => ast shouldEqual expectedOutput
      case Left(error) => fail(error.toString)
    }
  }
}
