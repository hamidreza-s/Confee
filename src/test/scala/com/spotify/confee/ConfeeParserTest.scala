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

    it("should parser type definitions with type items") {
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
          FactStmt(
            WordToken("foo"),
            TypeDef(Left(NameToken("Foo")), isList = false), FactItems(List.empty)
          ),
          FactStmt(
            WordToken("bar"),
            TypeDef(Right(WordToken("foo")), isList = false), FactItems(List.empty)
          )
        ))
      )
    }

    it("should parser fact definitions with fact items in one line") {
      assertAST(
        """fact alice : Person {name = "Alice" age = 20}""",
        Grammar(List(
          FactStmt(WordToken("alice"), TypeDef(Left(NameToken("Person")), isList = false),
            FactItems(List(
              FactItem(
                WordToken("name"),
                StringLiteral(StringToken("Alice"))
              ),
              FactItem(
                WordToken("age"),
                NumberLiteral(NumberToken(20.0))
              )
            ))
          )
        ))
      )
    }

    it("should parser fact definitions with string and number fact items") {
      assertAST(
        """fact alice : Person {
          |     name = "Alice"
          |     age = 20
          |}""".stripMargin,
        Grammar(List(
          FactStmt(WordToken("alice"), TypeDef(Left(NameToken("Person")), isList = false),
            FactItems(List(
              FactItem(
                WordToken("name"),
                StringLiteral(StringToken("Alice"))
              ),
              FactItem(
                WordToken("age"),
                NumberLiteral(NumberToken(20.0))
              )
            ))
          )
        ))
      )
    }

    it("should parser fact definitions with list of fact items") {
      assertAST(
        """fact team : Team {
          |     members = ["Alice", "Bob", "Joe"]
          |     records = [98, 97, 99]
          |}""".stripMargin,
        Grammar(List(
          FactStmt(WordToken("team"), TypeDef(Left(NameToken("Team")), isList = false),
            FactItems(List(
              FactItem(
                WordToken("members"),
                ListLiteral(List(
                  StringLiteral(StringToken("Alice")),
                  StringLiteral(StringToken("Bob")),
                  StringLiteral(StringToken("Joe"))
                ))
              ),
              FactItem(
                WordToken("records"),
                ListLiteral(List(
                  NumberLiteral(NumberToken(98.0)),
                  NumberLiteral(NumberToken(97.0)),
                  NumberLiteral(NumberToken(99.0))
                ))
              )
            ))
          )
        ))
      )
    }

    it("should parser fact definitions with list of list of fact items") {
      assertAST(
        """fact match : Match {
          |     players = [["Alice", "Bob"], ["Joe", "Monica"]]
          |     scores = [[7, 10], [23, 14]]
          |}""".stripMargin,
        Grammar(List(
          FactStmt(WordToken("match"), TypeDef(Left(NameToken("Match")), isList = false),
            FactItems(List(
              FactItem(
                WordToken("players"),
                ListLiteral(List(
                  ListLiteral(List(
                    StringLiteral(StringToken("Alice")),
                    StringLiteral(StringToken("Bob"))
                  )),
                  ListLiteral(List(
                    StringLiteral(StringToken("Joe")),
                    StringLiteral(StringToken("Monica"))
                  ))
                ))
              ),
              FactItem(
                WordToken("scores"),
                ListLiteral(List(
                  ListLiteral(List(
                    NumberLiteral(NumberToken(7.0)),
                    NumberLiteral(NumberToken(10.0))
                  )),
                  ListLiteral(List(
                    NumberLiteral(NumberToken(23.0)),
                    NumberLiteral(NumberToken(14.0))
                  ))
                ))
              )
            ))
          )
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
