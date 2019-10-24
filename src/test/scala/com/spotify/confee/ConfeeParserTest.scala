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
                LiteralString(StringToken("Alice"))
              ),
              FactItem(
                WordToken("age"),
                LiteralNumber(NumberToken(20.0))
              )
            ))
          )
        ))
      )
    }

    describe("Parser on literal expression") {

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
                  LiteralString(StringToken("Alice"))
                ),
                FactItem(
                  WordToken("age"),
                  LiteralNumber(NumberToken(20.0))
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
                  LiteralList(List(
                    LiteralString(StringToken("Alice")),
                    LiteralString(StringToken("Bob")),
                    LiteralString(StringToken("Joe"))
                  ))
                ),
                FactItem(
                  WordToken("records"),
                  LiteralList(List(
                    LiteralNumber(NumberToken(98.0)),
                    LiteralNumber(NumberToken(97.0)),
                    LiteralNumber(NumberToken(99.0))
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
                  LiteralList(List(
                    LiteralList(List(
                      LiteralString(StringToken("Alice")),
                      LiteralString(StringToken("Bob"))
                    )),
                    LiteralList(List(
                      LiteralString(StringToken("Joe")),
                      LiteralString(StringToken("Monica"))
                    ))
                  ))
                ),
                FactItem(
                  WordToken("scores"),
                  LiteralList(List(
                    LiteralList(List(
                      LiteralNumber(NumberToken(7.0)),
                      LiteralNumber(NumberToken(10.0))
                    )),
                    LiteralList(List(
                      LiteralNumber(NumberToken(23.0)),
                      LiteralNumber(NumberToken(14.0))
                    ))
                  ))
                )
              ))
            )
          ))
        )
      }
    }

    describe("Parser on arithmetic expression") {

      it("should parser fact definitions with arithmetic expression as fact item") {
        assertAST(
          """fact report : TimeReport {
            |     sec = 60
            |     hour = 60 * sec
            |     week = day * 7
            |     working_days = week - (2 * day)
            |}""".stripMargin,
          Grammar(List(
            FactStmt(WordToken("report"), TypeDef(Left(NameToken("TimeReport")), isList = false),
              FactItems(List(
                FactItem(WordToken("sec"), LiteralNumber(NumberToken(60.0))),
                FactItem(WordToken("hour"), ArithFactorGroup(
                  ArithMulOperator(),
                  ArithFactorNumber(NumberToken(60.0)),
                  ArithFactorWord(WordToken("sec"))
                )),

                FactItem(WordToken("week"), ArithFactorGroup(
                  ArithMulOperator(),
                  ArithFactorWord(WordToken("day")),
                  ArithFactorNumber(NumberToken(7.0))
                )),
                 FactItem(WordToken("working_days"), ArithFactorGroup(
                   ArithSubOperator(),
                   ArithFactorWord(WordToken("week")), ArithFactorGroup(
                     ArithMulOperator(),
                     ArithFactorNumber(NumberToken(2.0)),
                     ArithFactorWord(WordToken("day"))
                   )
                 ))
              ))
            )
          ))
        )
      }
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
