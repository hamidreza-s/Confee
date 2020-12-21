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
        Grammar(
          List(
            TypeStmt(NameToken("Foo"), TypeItems(List.empty)),
            TypeStmt(NameToken("Bar"), TypeItems(List.empty))
          )
        )
      )
    }

    it("should parse type definitions with type items in one line") {
      assertAST(
        "type Person { name: Text age: Int friends: [Person] }",
        Grammar(
          List(
            TypeStmt(
              NameToken("Person"),
              TypeItems(
                List(
                  TypeItem(WordToken("name"), TypeDef(Left(NameToken("Text")), isList = false)),
                  TypeItem(WordToken("age"), TypeDef(Left(NameToken("Int")), isList = false)),
                  TypeItem(WordToken("friends"), TypeDef(Left(NameToken("Person")), isList = true))
                )
              )
            )
          )
        )
      )
    }

    it("should parse type definitions with type items") {
      assertAST(
        """type Person {
          |     name: Text
          |     age: Int
          |     friends: [Person]
          |}""".stripMargin,
        Grammar(
          List(
            TypeStmt(
              NameToken("Person"),
              TypeItems(
                List(
                  TypeItem(WordToken("name"), TypeDef(Left(NameToken("Text")), isList = false)),
                  TypeItem(WordToken("age"), TypeDef(Left(NameToken("Int")), isList = false)),
                  TypeItem(WordToken("friends"), TypeDef(Left(NameToken("Person")), isList = true))
                )
              )
            )
          )
        )
      )
    }
  }

  describe("Parser on import statement") {
    it("should parse two import statements") {
      assertAST(
        """
          |import "/path/to/foo.confee"
          |import "/path/to/bar.confee"
        """.stripMargin,
        Grammar(
          List(
            ImportStmt(StringToken("/path/to/foo.confee")),
            ImportStmt(StringToken("/path/to/bar.confee"))
          )
        )
      )
    }
  }

  describe("Parser on conf statement") {

    it("should parse conf definition without conf items") {
      assertAST(
        """
          |conf foo : Foo { }
          |conf bar : foo { }
          |""".stripMargin,
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(List.empty)
            ),
            ConfStmt(
              WordToken("bar"),
              TypeDef(Right(WordToken("foo")), isList = false),
              ConfItems(List.empty)
            )
          )
        )
      )
    }

    it("should parse conf definition with conf items in one line") {
      assertAST(
        """conf alice : Person {name = "Alice" age = 20}""",
        Grammar(
          List(
            ConfStmt(
              WordToken("alice"),
              TypeDef(Left(NameToken("Person")), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    WordToken("name"),
                    LiteralStringFactor(StringToken("Alice"))
                  ),
                  ConfItem(
                    WordToken("age"),
                    LiteralNumberFactor(NumberToken(20.0))
                  )
                )
              )
            )
          )
        )
      )
    }

    describe("Parser on literal expression") {

      it("should parse conf definition with boolean, string and number") {
        assertAST(
          """conf alice : Person {
            |     name = "Alice"
            |     age = 20
            |     active = true
            |     flagged = false
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("alice"),
                TypeDef(Left(NameToken("Person")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("name"),
                      LiteralStringFactor(StringToken("Alice"))
                    ),
                    ConfItem(
                      WordToken("age"),
                      LiteralNumberFactor(NumberToken(20.0))
                    ),
                    ConfItem(
                      WordToken("active"),
                      LiteralBoolFactor(BoolToken(true))
                    ),
                    ConfItem(
                      WordToken("flagged"),
                      LiteralBoolFactor(BoolToken(false))
                    )
                  )
                )
              )
            )
          )
        )
      }

      it("should parse conf definition with operator in bool") {
        assertAST(
          """conf report : StatusReport {
            |     is_in_progress = false
            |     is_done = not false
            |     is_valid = true xor false
            |     is_successful = is_done and is_valid
            |     is_acceptable = (is_done and is_valid) or is_in_progress
            |     is_not_acceptable = not (is_done and is_valid) or is_in_progress
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("report"),
                TypeDef(Left(NameToken("StatusReport")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("is_in_progress"),
                      LiteralBoolFactor(BoolToken(false))
                    ),
                    ConfItem(
                      WordToken("is_done"),
                      LiteralBoolUnit(
                        LiteralBoolOperatorNot(),
                        LiteralBoolFactor(BoolToken(false))
                      )
                    ),
                    ConfItem(
                      WordToken("is_valid"),
                      LiteralBoolGroup(
                        LiteralBoolOperatorXor(),
                        LiteralBoolFactor(
                          BoolToken(true)
                        ),
                        LiteralBoolFactor(
                          BoolToken(false)
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("is_successful"),
                      LiteralBoolGroup(
                        LiteralBoolOperatorAnd(),
                        LiteralBoolWord(WordToken("is_done")),
                        LiteralBoolWord(WordToken("is_valid"))
                      )
                    ),
                    ConfItem(
                      WordToken("is_acceptable"),
                      LiteralBoolGroup(
                        LiteralBoolOperatorOr(),
                        LiteralBoolGroup(
                          LiteralBoolOperatorAnd(),
                          LiteralBoolWord(WordToken("is_done")),
                          LiteralBoolWord(WordToken("is_valid"))
                        ),
                        LiteralBoolWord(WordToken("is_in_progress"))
                      )
                    ),
                    ConfItem(
                      WordToken("is_not_acceptable"),
                      LiteralBoolUnit(
                        LiteralBoolOperatorNot(),
                        LiteralBoolGroup(
                          LiteralBoolOperatorOr(),
                          LiteralBoolGroup(
                            LiteralBoolOperatorAnd(),
                            LiteralBoolWord(WordToken("is_done")),
                            LiteralBoolWord(WordToken("is_valid"))
                          ),
                          LiteralBoolWord(WordToken("is_in_progress"))
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

      it("should parse conf definition with operator in number") {
        assertAST(
          """conf report : TimeReport {
            |     sec = 60
            |     hour = 60 * sec
            |     week = day * 7
            |     working_days = week - (2 * day)
            |     random = 1 + 2 + (3 * 4 / (5 - 6) + 7) - sec
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("report"),
                TypeDef(Left(NameToken("TimeReport")), isList = false),
                ConfItems(
                  List(
                    ConfItem(WordToken("sec"), LiteralNumberFactor(NumberToken(60.0))),
                    ConfItem(
                      WordToken("hour"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorMul(),
                        LiteralNumberFactor(NumberToken(60.0)),
                        LiteralNumberWord(WordToken("sec"))
                      )
                    ),
                    ConfItem(
                      WordToken("week"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorMul(),
                        LiteralNumberWord(WordToken("day")),
                        LiteralNumberFactor(NumberToken(7.0))
                      )
                    ),
                    ConfItem(
                      WordToken("working_days"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorSub(),
                        LiteralNumberWord(WordToken("week")),
                        LiteralNumberGroup(
                          LiteralNumberOperatorMul(),
                          LiteralNumberFactor(NumberToken(2.0)),
                          LiteralNumberWord(WordToken("day"))
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("random"),
                      LiteralNumberGroup(
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
                                    LiteralNumberFactor(NumberToken(6.0))
                                  ),
                                  LiteralNumberFactor(NumberToken(7.0))
                                )
                              )
                            ),
                            LiteralNumberWord(WordToken("sec"))
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

      it("should parse conf definition with operator in string") {
        assertAST(
          """conf report : SprintReport {
            |     project = "wheel"
            |     goal = "Inventing the " + project
            |     next = "Maintaining " + (goal - "Inventing ")
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("report"),
                TypeDef(Left(NameToken("SprintReport")), isList = false),
                ConfItems(
                  List(
                    ConfItem(WordToken("project"), LiteralStringFactor(StringToken("wheel"))),
                    ConfItem(
                      WordToken("goal"),
                      LiteralStringGroup(
                        LiteralStringOperatorConcat(),
                        LiteralStringFactor(StringToken("Inventing the ")),
                        LiteralStringWord(WordToken("project"))
                      )
                    ),
                    ConfItem(
                      WordToken("next"),
                      LiteralStringGroup(
                        LiteralStringOperatorConcat(),
                        LiteralStringFactor(StringToken("Maintaining ")),
                        LiteralStringGroup(
                          LiteralStringOperatorRemove(),
                          LiteralStringWord(WordToken("goal")),
                          LiteralStringFactor(StringToken("Inventing "))
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

      it("should parse conf definition with list") {
        assertAST(
          """conf team : Team {
            |     members = ["Alice", "Bob", "Joe"]
            |     records = [98, 97, 99]
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("team"),
                TypeDef(Left(NameToken("Team")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("members"),
                      LiteralArray(
                        List(
                          LiteralStringFactor(StringToken("Alice")),
                          LiteralStringFactor(StringToken("Bob")),
                          LiteralStringFactor(StringToken("Joe"))
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("records"),
                      LiteralArray(
                        List(
                          LiteralNumberFactor(NumberToken(98.0)),
                          LiteralNumberFactor(NumberToken(97.0)),
                          LiteralNumberFactor(NumberToken(99.0))
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

      it("should parse conf definition with list of list") {
        assertAST(
          """conf match : Match {
            |     players = [["Alice", "Bob"], ["Joe", "Monica"]]
            |     scores = [[7, 10], [23, 14]]
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(Left(NameToken("Match")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("players"),
                      LiteralArray(
                        List(
                          LiteralArray(
                            List(
                              LiteralStringFactor(StringToken("Alice")),
                              LiteralStringFactor(StringToken("Bob"))
                            )
                          ),
                          LiteralArray(
                            List(
                              LiteralStringFactor(StringToken("Joe")),
                              LiteralStringFactor(StringToken("Monica"))
                            )
                          )
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("scores"),
                      LiteralArray(
                        List(
                          LiteralArray(
                            List(
                              LiteralNumberFactor(NumberToken(7.0)),
                              LiteralNumberFactor(NumberToken(10.0))
                            )
                          ),
                          LiteralArray(
                            List(
                              LiteralNumberFactor(NumberToken(23.0)),
                              LiteralNumberFactor(NumberToken(14.0))
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

      it("should parse conf definition with object") {
        assertAST(
          """
            |conf match : Match {
            |     info = {
            |          stadium = "Azadi"
            |          capacity = 90000
            |          renovated = [2012, 2016]
            |     }
            |}
          """.stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(Left(NameToken("Match")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("info"),
                      LiteralObject(
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              WordToken("stadium"),
                              LiteralStringFactor(StringToken("Azadi"))
                            ),
                            LiteralObjectItem(
                              WordToken("capacity"),
                              LiteralNumberFactor(NumberToken(90000.0))
                            ),
                            LiteralObjectItem(
                              WordToken("renovated"),
                              LiteralArray(
                                List(
                                  LiteralNumberFactor(NumberToken(2012.0)),
                                  LiteralNumberFactor(NumberToken(2016.0))
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
          )
        )
      }

      it("should parse conf definition with object of object") {
        assertAST(
          """
            |conf match : Match {
            |     info = {
            |          size = {
            |               field = [110, 75]
            |               scoreboard = 104
            |          }
            |     }
            |}
          """.stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(Left(NameToken("Match")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("info"),
                      LiteralObject(
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              WordToken("size"),
                              LiteralObject(
                                LiteralObjectItems(
                                  List(
                                    LiteralObjectItem(
                                      WordToken("field"),
                                      LiteralArray(
                                        List(
                                          LiteralNumberFactor(NumberToken(110.0)),
                                          LiteralNumberFactor(NumberToken(75.0))
                                        )
                                      )
                                    ),
                                    LiteralObjectItem(
                                      WordToken("scoreboard"),
                                      LiteralNumberFactor(NumberToken(104.0))
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
              )
            )
          )
        )
      }

      it("should parse conf definition with proto") {
        assertAST(
          """
            |conf match : Match {
            |     info = templateInfo { quality = 5 }
            |}
          """.stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(Left(NameToken("Match")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("info"),
                      LiteralProto(
                        WordToken("templateInfo"),
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              WordToken("quality"),
                              LiteralNumberFactor(NumberToken(5.0))
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

      it("should parse conf definition with references") {
        assertAST(
          """conf foo : Foo {
            |     a1 = true
            |     b1 = "something"
            |     c1 = 123
            |     d1 = [1, 2, 3]
            |     e1 = { x = 1 y = 2 }
            |     f1 = e1 { z = 3 }
            |     a2 = a1
            |     b2 = b1
            |     c2 = c1
            |     d2 = d1
            |     e2 = e1
            |     f2 = f1
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("foo"),
                TypeDef(Left(NameToken("Foo")), isList = false),
                ConfItems(
                  List(
                    ConfItem(WordToken("a1"), LiteralBoolFactor(BoolToken(true))),
                    ConfItem(WordToken("b1"), LiteralStringFactor(StringToken("something"))),
                    ConfItem(WordToken("c1"), LiteralNumberFactor(NumberToken(123.0))),
                    ConfItem(
                      WordToken("d1"),
                      LiteralArray(
                        List(
                          LiteralNumberFactor(NumberToken(1.0)),
                          LiteralNumberFactor(NumberToken(2.0)),
                          LiteralNumberFactor(NumberToken(3.0))
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("e1"),
                      LiteralObject(
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              WordToken("x"),
                              LiteralNumberFactor(NumberToken(1.0))
                            ),
                            LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(2.0)))
                          )
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("f1"),
                      LiteralProto(
                        WordToken("e1"),
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(WordToken("z"), LiteralNumberFactor(NumberToken(3.0)))
                          )
                        )
                      )
                    ),
                    ConfItem(WordToken("a2"), LiteralNumberWord(WordToken("a1"))),
                    ConfItem(WordToken("b2"), LiteralNumberWord(WordToken("b1"))),
                    ConfItem(WordToken("c2"), LiteralNumberWord(WordToken("c1"))),
                    ConfItem(WordToken("d2"), LiteralNumberWord(WordToken("d1"))),
                    ConfItem(WordToken("e2"), LiteralNumberWord(WordToken("e1"))),
                    ConfItem(WordToken("f2"), LiteralNumberWord(WordToken("f1")))
                  )
                )
              )
            )
          )
        )
      }

      it("should parse conf definition with keyword in reference names") {
        assertAST(
          """conf foo : Foo {
            |     a = import_keyword + type_keyword + conf_keyword
            |     b = keyword_import + keyword_type + keyword_conf
            |     c = key_import_word + key_type_word + key_conf_word
            |     d = and_keyword + or_keyword + xor_keyword + not_keyword
            |     e = keyword_and + keyword_or + keyword_xor + keyword_not
            |     f = key_and_word + key_or_word + key_xor_word + key_not_word
            |}""".stripMargin,
          Grammar(
            List(
              ConfStmt(
                WordToken("foo"),
                TypeDef(Left(NameToken("Foo")), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      WordToken("a"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorAdd(),
                        LiteralNumberWord(WordToken("import_keyword")),
                        LiteralNumberGroup(
                          LiteralNumberOperatorAdd(),
                          LiteralNumberWord(WordToken("type_keyword")),
                          LiteralNumberWord(WordToken("conf_keyword"))
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("b"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorAdd(),
                        LiteralNumberWord(WordToken("keyword_import")),
                        LiteralNumberGroup(
                          LiteralNumberOperatorAdd(),
                          LiteralNumberWord(WordToken("keyword_type")),
                          LiteralNumberWord(WordToken("keyword_conf"))
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("c"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorAdd(),
                        LiteralNumberWord(WordToken("key_import_word")),
                        LiteralNumberGroup(
                          LiteralNumberOperatorAdd(),
                          LiteralNumberWord(WordToken("key_type_word")),
                          LiteralNumberWord(WordToken("key_conf_word"))
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("d"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorAdd(),
                        LiteralNumberWord(WordToken("and_keyword")),
                        LiteralNumberGroup(
                          LiteralNumberOperatorAdd(),
                          LiteralNumberWord(WordToken("or_keyword")),
                          LiteralNumberGroup(
                            LiteralNumberOperatorAdd(),
                            LiteralNumberWord(WordToken("xor_keyword")),
                            LiteralNumberWord(WordToken("not_keyword"))
                          )
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("e"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorAdd(),
                        LiteralNumberWord(WordToken("keyword_and")),
                        LiteralNumberGroup(
                          LiteralNumberOperatorAdd(),
                          LiteralNumberWord(WordToken("keyword_or")),
                          LiteralNumberGroup(
                            LiteralNumberOperatorAdd(),
                            LiteralNumberWord(WordToken("keyword_xor")),
                            LiteralNumberWord(WordToken("keyword_not"))
                          )
                        )
                      )
                    ),
                    ConfItem(
                      WordToken("f"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorAdd(),
                        LiteralNumberWord(WordToken("key_and_word")),
                        LiteralNumberGroup(
                          LiteralNumberOperatorAdd(),
                          LiteralNumberWord(WordToken("key_or_word")),
                          LiteralNumberGroup(
                            LiteralNumberOperatorAdd(),
                            LiteralNumberWord(WordToken("key_xor_word")),
                            LiteralNumberWord(WordToken("key_not_word"))
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
  }

  describe("Parser on a real example") {
    it("should parse type definitions") {
      assertAST(
        """
          |type DataInfo {
          |     id: Text
          |     description: Text
          |     facts: DataFact
          |}
          |
          |type DataFact {
          |     doc: Text
          |     workFlows: [DataWorkflow]
          |}
          |
          |type DataWorkflow {
          |     id: Text
          |     account: Text
          |     schedule: Text
          |     dockerArgs: [String]
          |}
        """.stripMargin,
        Grammar(
          List(
            TypeStmt(
              NameToken("DataInfo"),
              TypeItems(
                List(
                  TypeItem(WordToken("id"), TypeDef(Left(NameToken("Text")), isList = false)),
                  TypeItem(
                    WordToken("description"),
                    TypeDef(Left(NameToken("Text")), isList = false)
                  ),
                  TypeItem(WordToken("facts"), TypeDef(Left(NameToken("DataFact")), isList = false))
                )
              )
            ),
            TypeStmt(
              NameToken("DataFact"),
              TypeItems(
                List(
                  TypeItem(WordToken("doc"), TypeDef(Left(NameToken("Text")), isList = false)),
                  TypeItem(
                    WordToken("workFlows"),
                    TypeDef(Left(NameToken("DataWorkflow")), isList = true)
                  )
                )
              )
            ),
            TypeStmt(
              NameToken("DataWorkflow"),
              TypeItems(
                List(
                  TypeItem(WordToken("id"), TypeDef(Left(NameToken("Text")), isList = false)),
                  TypeItem(WordToken("account"), TypeDef(Left(NameToken("Text")), isList = false)),
                  TypeItem(WordToken("schedule"), TypeDef(Left(NameToken("Text")), isList = false)),
                  TypeItem(
                    WordToken("dockerArgs"),
                    TypeDef(Left(NameToken("String")), isList = true)
                  )
                )
              )
            )
          )
        )
      )
    }

    it("should parse conf definitions based on the above types ") {
      assertAST(
        """
          |import "/path/to/DataInfoTypes.confee"
          |
          |conf workflow : DataWorkflow {
          |     account = "admin@dataflow.com"
          |     dockerArgs = ["--wrap-luigi"]
          |}
          |
          |conf dataInfo : DataInfo {
          |     id = "e73d6402"
          |     description = "sample desc"
          |     facts = {
          |          doc = "sample doc"
          |          workFlows = [
          |               workflow { id = "a1dc6109" schedule = "monthly" },
          |               workflow { id = "320a0de1" schedule = "monthly" },
          |               workflow { id = "ac62a310" schedule = "daily" },
          |               workflow { id = "68b703f8" schedule = "daily" }
          |          ]
          |     }
          |}
        """.stripMargin,
        Grammar(
          List(
            ImportStmt(StringToken("/path/to/DataInfoTypes.confee")),
            ConfStmt(
              WordToken("workflow"),
              TypeDef(Left(NameToken("DataWorkflow")), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    WordToken("account"),
                    LiteralStringFactor(StringToken("admin@dataflow.com"))
                  ),
                  ConfItem(
                    WordToken("dockerArgs"),
                    LiteralArray(
                      List(
                        LiteralStringFactor(StringToken("--wrap-luigi"))
                      )
                    )
                  )
                )
              )
            ),
            ConfStmt(
              WordToken("dataInfo"),
              TypeDef(Left(NameToken("DataInfo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("id"), LiteralStringFactor(StringToken("e73d6402"))),
                  ConfItem(
                    WordToken("description"),
                    LiteralStringFactor(StringToken("sample desc"))
                  ),
                  ConfItem(
                    WordToken("facts"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("doc"),
                            LiteralStringFactor(StringToken("sample doc"))
                          ),
                          LiteralObjectItem(
                            WordToken("workFlows"),
                            LiteralArray(
                              List(
                                LiteralProto(
                                  WordToken("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        WordToken("id"),
                                        LiteralStringFactor(StringToken("a1dc6109"))
                                      ),
                                      LiteralObjectItem(
                                        WordToken("schedule"),
                                        LiteralStringFactor(StringToken("monthly"))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  WordToken("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        WordToken("id"),
                                        LiteralStringFactor(StringToken("320a0de1"))
                                      ),
                                      LiteralObjectItem(
                                        WordToken("schedule"),
                                        LiteralStringFactor(StringToken("monthly"))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  WordToken("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        WordToken("id"),
                                        LiteralStringFactor(StringToken("ac62a310"))
                                      ),
                                      LiteralObjectItem(
                                        WordToken("schedule"),
                                        LiteralStringFactor(StringToken("daily"))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  WordToken("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        WordToken("id"),
                                        LiteralStringFactor(StringToken("68b703f8"))
                                      ),
                                      LiteralObjectItem(
                                        WordToken("schedule"),
                                        LiteralStringFactor(StringToken("daily"))
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
                )
              )
            )
          )
        )
      )
    }
  }

  def assertAST(input: String, expectedOutput: ConfeeAST): Unit = {
    (for {
      tokens <- ConfeeLexer(input).right
      ast    <- ConfeeParser(tokens)
    } yield ast) match {
      case Right(ast)  => ast shouldEqual expectedOutput
      case Left(error) => fail(error.toString)
    }
  }
}
