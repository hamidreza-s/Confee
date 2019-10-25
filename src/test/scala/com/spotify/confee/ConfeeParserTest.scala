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

  describe("Parser on conf statement") {

    it("should parse conf definitions without conf items") {
      assertAST(
        """
          |conf foo : Foo { }
          |conf bar : foo { }
          |""".stripMargin,
        Grammar(List(
          ConfStmt(
            WordToken("foo"),
            TypeDef(Left(NameToken("Foo")), isList = false), ConfItems(List.empty)
          ),
          ConfStmt(
            WordToken("bar"),
            TypeDef(Right(WordToken("foo")), isList = false), ConfItems(List.empty)
          )
        ))
      )
    }

    it("should parser conf definitions with conf items in one line") {
      assertAST(
        """conf alice : Person {name = "Alice" age = 20}""",
        Grammar(List(
          ConfStmt(WordToken("alice"), TypeDef(Left(NameToken("Person")), isList = false),
            ConfItems(List(
              ConfItem(
                WordToken("name"),
                LiteralStringFactor(StringToken("Alice"))
              ),
              ConfItem(
                WordToken("age"),
                LiteralNumberFactor(NumberToken(20.0))
              )
            ))
          )
        ))
      )
    }

    describe("Parser on literal expression") {

      it("should parser conf definitions with string and number conf items") {
        assertAST(
          """conf alice : Person {
            |     name = "Alice"
            |     age = 20
            |}""".stripMargin,
          Grammar(List(
            ConfStmt(WordToken("alice"), TypeDef(Left(NameToken("Person")), isList = false),
              ConfItems(List(
                ConfItem(
                  WordToken("name"),
                  LiteralStringFactor(StringToken("Alice"))
                ),
                ConfItem(
                  WordToken("age"),
                  LiteralNumberFactor(NumberToken(20.0))
                )
              ))
            )
          ))
        )
      }

      it("should parser conf definitions with list of conf items") {
        assertAST(
          """conf team : Team {
            |     members = ["Alice", "Bob", "Joe"]
            |     records = [98, 97, 99]
            |}""".stripMargin,
          Grammar(List(
            ConfStmt(WordToken("team"), TypeDef(Left(NameToken("Team")), isList = false),
              ConfItems(List(
                ConfItem(
                  WordToken("members"),
                  LiteralArray(List(
                    LiteralStringFactor(StringToken("Alice")),
                    LiteralStringFactor(StringToken("Bob")),
                    LiteralStringFactor(StringToken("Joe"))
                  ))
                ),
                ConfItem(
                  WordToken("records"),
                  LiteralArray(List(
                    LiteralNumberFactor(NumberToken(98.0)),
                    LiteralNumberFactor(NumberToken(97.0)),
                    LiteralNumberFactor(NumberToken(99.0))
                  ))
                )
              ))
            )
          ))
        )
      }

      it("should parser conf definitions with list of list of conf items") {
        assertAST(
          """conf match : Match {
            |     players = [["Alice", "Bob"], ["Joe", "Monica"]]
            |     scores = [[7, 10], [23, 14]]
            |}""".stripMargin,
          Grammar(List(
            ConfStmt(WordToken("match"), TypeDef(Left(NameToken("Match")), isList = false),
              ConfItems(List(
                ConfItem(
                  WordToken("players"),
                  LiteralArray(List(
                    LiteralArray(List(
                      LiteralStringFactor(StringToken("Alice")),
                      LiteralStringFactor(StringToken("Bob"))
                    )),
                    LiteralArray(List(
                      LiteralStringFactor(StringToken("Joe")),
                      LiteralStringFactor(StringToken("Monica"))
                    ))
                  ))
                ),
                ConfItem(
                  WordToken("scores"),
                  LiteralArray(List(
                    LiteralArray(List(
                      LiteralNumberFactor(NumberToken(7.0)),
                      LiteralNumberFactor(NumberToken(10.0))
                    )),
                    LiteralArray(List(
                      LiteralNumberFactor(NumberToken(23.0)),
                      LiteralNumberFactor(NumberToken(14.0))
                    ))
                  ))
                )
              ))
            )
          ))
        )
      }

      it("should parser conf definitions with arithmetic in number expression as conf item") {
        assertAST(
          """conf report : TimeReport {
            |     sec = 60
            |     hour = 60 * sec
            |     week = day * 7
            |     working_days = week - (2 * day)
            |     random = 1 + 2 + (3 * 4 / (5 - 6) + 7) - sec
            |}""".stripMargin,
          Grammar(List(
            ConfStmt(WordToken("report"), TypeDef(Left(NameToken("TimeReport")), isList = false),
              ConfItems(List(
                ConfItem(WordToken("sec"), LiteralNumberFactor(NumberToken(60.0))),
                ConfItem(WordToken("hour"), LiteralNumberGroup(
                  LiteralNumberOperatorMul(),
                  LiteralNumberFactor(NumberToken(60.0)),
                  LiteralNumberWord(WordToken("sec"))
                )),

                ConfItem(WordToken("week"), LiteralNumberGroup(
                  LiteralNumberOperatorMul(),
                  LiteralNumberWord(WordToken("day")),
                  LiteralNumberFactor(NumberToken(7.0))
                )),
                ConfItem(WordToken("working_days"), LiteralNumberGroup(
                  LiteralNumberOperatorSub(),
                  LiteralNumberWord(WordToken("week")), LiteralNumberGroup(
                    LiteralNumberOperatorMul(),
                    LiteralNumberFactor(NumberToken(2.0)),
                    LiteralNumberWord(WordToken("day"))
                  )
                )),
                ConfItem(WordToken("random"), LiteralNumberGroup(
                  LiteralNumberOperatorAdd(),
                  LiteralNumberFactor(NumberToken(1.0)),
                  LiteralNumberGroup(
                    LiteralNumberOperatorAdd(),
                    LiteralNumberFactor(NumberToken(2.0)),
                    LiteralNumberGroup(
                      LiteralNumberOperatorSub(),
                      LiteralNumberGroup(
                        LiteralNumberOperatorMul(),
                        LiteralNumberFactor(NumberToken(3.0)),
                        LiteralNumberGroup(
                          LiteralNumberOperatorDiv(),
                          LiteralNumberFactor(NumberToken(4.0)),
                          LiteralNumberGroup(
                            LiteralNumberOperatorAdd(),
                            LiteralNumberGroup(
                              LiteralNumberOperatorSub(),
                              LiteralNumberFactor(NumberToken(5.0)),
                              LiteralNumberFactor(NumberToken(6.0))),
                            LiteralNumberFactor(NumberToken(7.0))))),
                      LiteralNumberWord(WordToken("sec"))
                    )
                  )
                ))
              ))
            )
          ))
        )
      }

      it("should parser conf definitions with concat/remove in string expression as conf item") {
        assertAST(
          """conf report : SprintReport {
            |     project = "wheel"
            |     goal = "Inventing the " + project
            |     next = "Maintaining " + (goal - "Inventing ")
            |}""".stripMargin,
          Grammar(List(
            ConfStmt(WordToken("report"), TypeDef(Left(NameToken("SprintReport")), isList = false),
              ConfItems(List(
                ConfItem(WordToken("project"), LiteralStringFactor(StringToken("wheel"))),
                ConfItem(WordToken("goal"), LiteralStringGroup(
                  LiteralStringOperatorConcat(),
                  LiteralStringFactor(StringToken("Inventing the ")),
                  LiteralStringWord(WordToken("project"))
                )),
                ConfItem(WordToken("next"), LiteralStringGroup(
                  LiteralStringOperatorConcat(),
                  LiteralStringFactor(StringToken("Maintaining ")), LiteralStringGroup(
                    LiteralStringOperatorRemove(),
                    LiteralStringWord(WordToken("goal")),
                    LiteralStringFactor(StringToken("Inventing "))
                  ))
                )
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
