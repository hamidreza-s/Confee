package com.spotify.confee

import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}

class ConfeeParserTest extends FunSpec with Matchers with BeforeAndAfterEach {

  describe("Parser on statement") {

    it("should parse different types of statement") {
      assertAST(
        """
          |type Foo { }
          |type Bar { }
          |fact foo : Foo { }
          |fact bar : foo { }
          |""".stripMargin,
        Stmts(List(
          TypeStmt(NameToken("Foo"), List()),
          TypeStmt(NameToken("Bar"), List()),
          FactStmt(WordToken("foo"), TypeDef(Left(NameToken("Foo")), false), List()),
          FactStmt(WordToken("bar"), TypeDef(Right(WordToken("foo")), false), List())
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
