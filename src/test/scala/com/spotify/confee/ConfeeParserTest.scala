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
          TypeStmt(NameToken("Foo"), TypeItems(List.empty)),
          TypeStmt(NameToken("Bar"), TypeItems(List.empty))
        ))
      )
    }

    it("should parser type definitions with type items in one line") {
      assertAST(
        "type Person { name: Text age: Int friends: [Person] }",
        Grammar(List(
          TypeStmt(NameToken("Person"), TypeItems(List(
            TypeItem(WordToken("name"), TypeDef(Left(NameToken("Text")), isList = false)),
            TypeItem(WordToken("age"), TypeDef(Left(NameToken("Int")), isList = false)),
            TypeItem(WordToken("friends"), TypeDef(Left(NameToken("Person")), isList = true))
          )))
        ))
      )
    }

    it("should parser type definitions with type items in multiple lines") {
      assertAST(
        """type Person {
          |     name: Text
          |     age: Int
          |     friends: [Person]
          |}""".stripMargin,
        Grammar(List(
          TypeStmt(NameToken("Person"), TypeItems(List(
            TypeItem(WordToken("name"), TypeDef(Left(NameToken("Text")), isList = false)),
            TypeItem(WordToken("age"), TypeDef(Left(NameToken("Int")), isList = false)),
            TypeItem(WordToken("friends"), TypeDef(Left(NameToken("Person")), isList = true))
          )))
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
