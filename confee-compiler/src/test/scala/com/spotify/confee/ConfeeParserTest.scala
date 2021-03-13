package com.spotify.confee

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeParserTest extends AnyFunSpec with Matchers with BeforeAndAfterEach {

  describe("Parser on type statement") {

    it("should parse type definitions without type items") {
      parsedAST("""
          |type Foo { }
          |type Bar { }
          |""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            TypeStmt(NameToken("Foo"), TypeItems(List.empty)),
            TypeStmt(NameToken("Bar"), TypeItems(List.empty))
          )
        )
      )
    }

    it("should parse type definitions with type items in one line") {
      parsedAST("type Person { name: Text age: Int friends: [Person] }") shouldEqual Right(
        Grammar(
          List(
            TypeStmt(
              NameToken("Person"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("name"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(TypeItemKey("age"), TypeDef(NameToken("Int"), isList = false)),
                  TypeItem(TypeItemKey("friends"), TypeDef(NameToken("Person"), isList = true))
                )
              )
            )
          )
        )
      )
    }

    it("should parse type definitions with type item keys starting with lowercase") {
      parsedAST("""type Person {
          |     name: Text
          |     age: Int
          |     friends: [Person]
          |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            TypeStmt(
              NameToken("Person"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("name"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(TypeItemKey("age"), TypeDef(NameToken("Int"), isList = false)),
                  TypeItem(TypeItemKey("friends"), TypeDef(NameToken("Person"), isList = true))
                )
              )
            )
          )
        )
      )
    }

    it("should parse type definitions with type key starting with uppercase") {
      parsedAST("""type Person {
                  |     Name: Text
                  |     Age: Int
                  |     Friends: [Person]
                  |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            TypeStmt(
              NameToken("Person"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("Name"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(TypeItemKey("Age"), TypeDef(NameToken("Int"), isList = false)),
                  TypeItem(TypeItemKey("Friends"), TypeDef(NameToken("Person"), isList = true))
                )
              )
            )
          )
        )
      )
    }

    it("should NOT parse type definitions with a name which starts with lowercase") {
      parsedAST("""type person {
          |     name: Text
          |}""".stripMargin) shouldEqual Left(ConfeeParserError(Location(1, 6), "name expected"))
    }
  }

  describe("Parser on import statement") {
    it("should parse two import statements") {
      parsedAST("""
          |import "/path/to/foo.confee"
          |import "/path/to/bar.confee"
        """.stripMargin) shouldEqual Right(
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
      parsedAST("""
          |conf foo : Foo { }
          |conf bar : Bar { }
          |""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(List.empty)
            ),
            ConfStmt(
              WordToken("bar"),
              TypeDef(NameToken("Bar"), isList = false),
              ConfItems(List.empty)
            )
          )
        )
      )
    }

    it("should parse conf definition with conf items in one line") {
      parsedAST("""conf alice : Person {name = "Alice" age = 20}""") shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("alice"),
              TypeDef(NameToken("Person"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("name"),
                    LiteralStringFactor(StringToken("Alice"))
                  ),
                  ConfItem(
                    ConfItemKey("age"),
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
        parsedAST("""conf alice : Person {
            |     name = "Alice"
            |     age = 20
            |     active = true
            |     flagged = false
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("alice"),
                TypeDef(NameToken("Person"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("name"),
                      LiteralStringFactor(StringToken("Alice"))
                    ),
                    ConfItem(
                      ConfItemKey("age"),
                      LiteralNumberFactor(NumberToken(20.0))
                    ),
                    ConfItem(
                      ConfItemKey("active"),
                      LiteralBoolFactor(BoolToken(true))
                    ),
                    ConfItem(
                      ConfItemKey("flagged"),
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
        parsedAST("""conf report : StatusReport {
            |     is_in_progress = false
            |     is_done = not false
            |     is_valid = true xor false
            |     is_successful = is_done and is_valid
            |     is_acceptable = (is_done and is_valid) or is_in_progress
            |     is_not_acceptable = not (is_done and is_valid) or is_in_progress
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("report"),
                TypeDef(NameToken("StatusReport"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("is_in_progress"),
                      LiteralBoolFactor(BoolToken(false))
                    ),
                    ConfItem(
                      ConfItemKey("is_done"),
                      LiteralBoolUnit(
                        LiteralBoolOperatorNot(),
                        LiteralBoolFactor(BoolToken(false))
                      )
                    ),
                    ConfItem(
                      ConfItemKey("is_valid"),
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
                      ConfItemKey("is_successful"),
                      LiteralBoolGroup(
                        LiteralBoolOperatorAnd(),
                        LiteralBoolWord(WordToken("is_done")),
                        LiteralBoolWord(WordToken("is_valid"))
                      )
                    ),
                    ConfItem(
                      ConfItemKey("is_acceptable"),
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
                      ConfItemKey("is_not_acceptable"),
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
        parsedAST("""conf report : TimeReport {
            |     sec = 60
            |     hour = 60 * sec
            |     week = day * 7
            |     working_days = week - (2 * day)
            |     random = 1 + 2 + (3 * 4 / (5 - 6) + 7) - sec
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("report"),
                TypeDef(NameToken("TimeReport"), isList = false),
                ConfItems(
                  List(
                    ConfItem(ConfItemKey("sec"), LiteralNumberFactor(NumberToken(60.0))),
                    ConfItem(
                      ConfItemKey("hour"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorMul(),
                        LiteralNumberFactor(NumberToken(60.0)),
                        LiteralNumberWord(WordToken("sec"))
                      )
                    ),
                    ConfItem(
                      ConfItemKey("week"),
                      LiteralNumberGroup(
                        LiteralNumberOperatorMul(),
                        LiteralNumberWord(WordToken("day")),
                        LiteralNumberFactor(NumberToken(7.0))
                      )
                    ),
                    ConfItem(
                      ConfItemKey("working_days"),
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
                      ConfItemKey("random"),
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
        parsedAST("""conf report : SprintReport {
            |     project = "wheel"
            |     goal = "Inventing the " + project
            |     next = "Maintaining " + (goal - "Inventing ")
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("report"),
                TypeDef(NameToken("SprintReport"), isList = false),
                ConfItems(
                  List(
                    ConfItem(ConfItemKey("project"), LiteralStringFactor(StringToken("wheel"))),
                    ConfItem(
                      ConfItemKey("goal"),
                      LiteralStringGroup(
                        LiteralStringOperatorConcat(),
                        LiteralStringFactor(StringToken("Inventing the ")),
                        LiteralStringWord(WordToken("project"))
                      )
                    ),
                    ConfItem(
                      ConfItemKey("next"),
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
        parsedAST("""conf team : Team {
            |     members = ["Alice", "Bob", "Joe"]
            |     records = [98, 97, 99]
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("team"),
                TypeDef(NameToken("Team"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("members"),
                      LiteralArray(
                        List(
                          LiteralStringFactor(StringToken("Alice")),
                          LiteralStringFactor(StringToken("Bob")),
                          LiteralStringFactor(StringToken("Joe"))
                        )
                      )
                    ),
                    ConfItem(
                      ConfItemKey("records"),
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
        parsedAST("""conf match : Match {
            |     players = [["Alice", "Bob"], ["Joe", "Monica"]]
            |     scores = [[7, 10], [23, 14]]
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(NameToken("Match"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("players"),
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
                      ConfItemKey("scores"),
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
        parsedAST("""
            |conf match : Match {
            |     info = {
            |          stadium = "Azadi"
            |          capacity = 90000
            |          renovated = [2012, 2016]
            |     }
            |}
          """.stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(NameToken("Match"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("info"),
                      LiteralObject(
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              LiteralObjectItemKey("stadium"),
                              LiteralStringFactor(StringToken("Azadi"))
                            ),
                            LiteralObjectItem(
                              LiteralObjectItemKey("capacity"),
                              LiteralNumberFactor(NumberToken(90000.0))
                            ),
                            LiteralObjectItem(
                              LiteralObjectItemKey("renovated"),
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
        parsedAST("""
            |conf match : Match {
            |     info = {
            |          size = {
            |               field = [110, 75]
            |               scoreboard = 104
            |          }
            |     }
            |}
          """.stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(NameToken("Match"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("info"),
                      LiteralObject(
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              LiteralObjectItemKey("size"),
                              LiteralObject(
                                LiteralObjectItems(
                                  List(
                                    LiteralObjectItem(
                                      LiteralObjectItemKey("field"),
                                      LiteralArray(
                                        List(
                                          LiteralNumberFactor(NumberToken(110.0)),
                                          LiteralNumberFactor(NumberToken(75.0))
                                        )
                                      )
                                    ),
                                    LiteralObjectItem(
                                      LiteralObjectItemKey("scoreboard"),
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
        parsedAST("""
            |conf match : Match {
            |     info = templateInfo { quality = 5 }
            |}
          """.stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("match"),
                TypeDef(NameToken("Match"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("info"),
                      LiteralProto(
                        LiteralProtoKey("templateInfo"),
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              LiteralObjectItemKey("quality"),
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
        parsedAST("""conf foo : Foo {
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
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("foo"),
                TypeDef(NameToken("Foo"), isList = false),
                ConfItems(
                  List(
                    ConfItem(ConfItemKey("a1"), LiteralBoolFactor(BoolToken(true))),
                    ConfItem(ConfItemKey("b1"), LiteralStringFactor(StringToken("something"))),
                    ConfItem(ConfItemKey("c1"), LiteralNumberFactor(NumberToken(123.0))),
                    ConfItem(
                      ConfItemKey("d1"),
                      LiteralArray(
                        List(
                          LiteralNumberFactor(NumberToken(1.0)),
                          LiteralNumberFactor(NumberToken(2.0)),
                          LiteralNumberFactor(NumberToken(3.0))
                        )
                      )
                    ),
                    ConfItem(
                      ConfItemKey("e1"),
                      LiteralObject(
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              LiteralObjectItemKey("x"),
                              LiteralNumberFactor(NumberToken(1.0))
                            ),
                            LiteralObjectItem(
                              LiteralObjectItemKey("y"),
                              LiteralNumberFactor(NumberToken(2.0))
                            )
                          )
                        )
                      )
                    ),
                    ConfItem(
                      ConfItemKey("f1"),
                      LiteralProto(
                        LiteralProtoKey("e1"),
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              LiteralObjectItemKey("z"),
                              LiteralNumberFactor(NumberToken(3.0))
                            )
                          )
                        )
                      )
                    ),
                    ConfItem(ConfItemKey("a2"), LiteralWord(WordToken("a1"))),
                    ConfItem(ConfItemKey("b2"), LiteralWord(WordToken("b1"))),
                    ConfItem(ConfItemKey("c2"), LiteralWord(WordToken("c1"))),
                    ConfItem(ConfItemKey("d2"), LiteralWord(WordToken("d1"))),
                    ConfItem(ConfItemKey("e2"), LiteralWord(WordToken("e1"))),
                    ConfItem(ConfItemKey("f2"), LiteralWord(WordToken("f1")))
                  )
                )
              )
            )
          )
        )
      }

      it("should parse conf definition with keyword in reference names") {
        parsedAST("""conf foo : Foo {
            |     a = import_keyword + type_keyword + conf_keyword
            |     b = keyword_import + keyword_type + keyword_conf
            |     c = key_import_word + key_type_word + key_conf_word
            |     d = and_keyword + or_keyword + xor_keyword + not_keyword
            |     e = keyword_and + keyword_or + keyword_xor + keyword_not
            |     f = key_and_word + key_or_word + key_xor_word + key_not_word
            |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("foo"),
                TypeDef(NameToken("Foo"), isList = false),
                ConfItems(
                  List(
                    ConfItem(
                      ConfItemKey("a"),
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
                      ConfItemKey("b"),
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
                      ConfItemKey("c"),
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
                      ConfItemKey("d"),
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
                      ConfItemKey("e"),
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
                      ConfItemKey("f"),
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

      it("should parse conf / object / proto definitions with item key starting with uppercase") {
        parsedAST("""conf foo : Foo {
                    |     A = true
                    |     B = [true]
                    |     C = { Y = true }
                    |     E = C { X = true }
                    |}""".stripMargin) shouldEqual Right(
          Grammar(
            List(
              ConfStmt(
                WordToken("foo"),
                TypeDef(NameToken("Foo"), isList = false),
                ConfItems(
                  List(
                    ConfItem(ConfItemKey("A"), LiteralBoolFactor(BoolToken(true))),
                    ConfItem(
                      ConfItemKey("B"),
                      LiteralArray(List(LiteralBoolFactor(BoolToken(true))))
                    ),
                    ConfItem(
                      ConfItemKey("C"),
                      LiteralObject(
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              LiteralObjectItemKey("Y"),
                              LiteralBoolFactor(BoolToken(true))
                            )
                          )
                        )
                      )
                    ),
                    ConfItem(
                      ConfItemKey("E"),
                      LiteralProto(
                        LiteralProtoKey("C"),
                        LiteralObjectItems(
                          List(
                            LiteralObjectItem(
                              LiteralObjectItemKey("X"),
                              LiteralBoolFactor(BoolToken(true))
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
  }

  describe("Parser on a real example") {
    it("should parse type definitions") {
      parsedAST("""
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
        """.stripMargin) shouldEqual Right(
        Grammar(
          List(
            TypeStmt(
              NameToken("DataInfo"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("id"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(
                    TypeItemKey("description"),
                    TypeDef(NameToken("Text"), isList = false)
                  ),
                  TypeItem(TypeItemKey("facts"), TypeDef(NameToken("DataFact"), isList = false))
                )
              )
            ),
            TypeStmt(
              NameToken("DataFact"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("doc"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(
                    TypeItemKey("workFlows"),
                    TypeDef(NameToken("DataWorkflow"), isList = true)
                  )
                )
              )
            ),
            TypeStmt(
              NameToken("DataWorkflow"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("id"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(TypeItemKey("account"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(TypeItemKey("schedule"), TypeDef(NameToken("Text"), isList = false)),
                  TypeItem(
                    TypeItemKey("dockerArgs"),
                    TypeDef(NameToken("String"), isList = true)
                  )
                )
              )
            )
          )
        )
      )
    }

    it("should parse conf definitions based on the above types ") {
      parsedAST("""
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
        """.stripMargin) shouldEqual Right(
        Grammar(
          List(
            ImportStmt(StringToken("/path/to/DataInfoTypes.confee")),
            ConfStmt(
              WordToken("workflow"),
              TypeDef(NameToken("DataWorkflow"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("account"),
                    LiteralStringFactor(StringToken("admin@dataflow.com"))
                  ),
                  ConfItem(
                    ConfItemKey("dockerArgs"),
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
              TypeDef(NameToken("DataInfo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("id"), LiteralStringFactor(StringToken("e73d6402"))),
                  ConfItem(
                    ConfItemKey("description"),
                    LiteralStringFactor(StringToken("sample desc"))
                  ),
                  ConfItem(
                    ConfItemKey("facts"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("doc"),
                            LiteralStringFactor(StringToken("sample doc"))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("workFlows"),
                            LiteralArray(
                              List(
                                LiteralProto(
                                  LiteralProtoKey("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("id"),
                                        LiteralStringFactor(StringToken("a1dc6109"))
                                      ),
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("schedule"),
                                        LiteralStringFactor(StringToken("monthly"))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  LiteralProtoKey("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("id"),
                                        LiteralStringFactor(StringToken("320a0de1"))
                                      ),
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("schedule"),
                                        LiteralStringFactor(StringToken("monthly"))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  LiteralProtoKey("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("id"),
                                        LiteralStringFactor(StringToken("ac62a310"))
                                      ),
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("schedule"),
                                        LiteralStringFactor(StringToken("daily"))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  LiteralProtoKey("workflow"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("id"),
                                        LiteralStringFactor(StringToken("68b703f8"))
                                      ),
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("schedule"),
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

  def parsedAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens <- ConfeeLexer(input)
      ast    <- ConfeeParser(tokens)
    } yield ast
  }
}
