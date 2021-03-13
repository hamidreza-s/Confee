package com.spotify.confee

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeBinderTest extends AnyFunSpec with Matchers {

  describe("Binder on referenced bool") {
    it("should bind a referenced bool if it is defined in scope") {
      bindAST("""conf foo : Foo {
            |     bar = true
            |     bat = bar and false
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(
                    WordToken("bat"),
                    LiteralBoolGroup(
                      LiteralBoolOperatorAnd(),
                      LiteralBoolFactor(BoolToken(true)),
                      LiteralBoolFactor(BoolToken(false))
                    )
                  )
                )
              )
            )
          )
        )
      )
    }

    it("should NOT bind a referenced bool if it is NOT defined in scope") {
      bindAST("""conf foo : Foo {
            |     bar = true and not_defined_reference
            |}""".stripMargin) shouldEqual Left(
        ConfeeBinderError(
          Location(2, 21),
          "Reference error: 'not_defined_reference' is not defined"
        )
      )
    }
  }

  describe("Binder on referenced string and number") {
    it("should bind a referenced string and number if it is defined in scope") {
      bindAST("""conf foo : Foo {
            |     bar = "abc"
            |     bat = 123
            |     ban = "def" + bar
            |     baz = 456 + bat
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("abc"))),
                  ConfItem(WordToken("bat"), LiteralNumberFactor(NumberToken(123.0))),
                  ConfItem(
                    WordToken("ban"),
                    LiteralStringGroup(
                      LiteralStringOperatorConcat(),
                      LiteralStringFactor(StringToken("def")),
                      LiteralStringFactor(StringToken("abc"))
                    )
                  ),
                  ConfItem(
                    WordToken("baz"),
                    LiteralNumberGroup(
                      LiteralNumberOperatorAdd(),
                      LiteralNumberFactor(NumberToken(456.0)),
                      LiteralNumberFactor(NumberToken(123.0))
                    )
                  )
                )
              )
            )
          )
        )
      )
    }

    it("should NOT bind a referenced string if it is not defined in scope") {
      bindAST("""conf foo : Foo {
            |     bar = "abc" + not_defined_reference
            |}""".stripMargin) shouldEqual Left(
        ConfeeBinderError(
          Location(2, 20),
          "Reference error: 'not_defined_reference' is not defined"
        )
      )
    }

    it("should NOT bind a referenced number if it is not defined in scope") {
      bindAST("""conf foo : Foo {
            |     bar = 123 + not_defined_reference
            |}""".stripMargin) shouldEqual Left(
        ConfeeBinderError(
          Location(2, 18),
          "Reference error: 'not_defined_reference' is not defined"
        )
      )
    }
  }

  describe("Binder on referenced array, object and proto") {
    it("should bind a referenced array, object and proto if it is defined in scope") {
      bindAST("""conf foo : Foo {
            |     bar = [1,2,3]
            |     bat = { a = 1 b = 2 }
            |     ban = bat { c = 3 }
            |     baz = bar
            |     bal = bat
            |     bay = ban
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    WordToken("bar"),
                    LiteralArray(
                      List(
                        LiteralNumberFactor(NumberToken(1.0)),
                        LiteralNumberFactor(NumberToken(2.0)),
                        LiteralNumberFactor(NumberToken(3.0))
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("bat"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("a"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(WordToken("b"), LiteralNumberFactor(NumberToken(2.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("ban"),
                    LiteralProto(
                      WordToken("bat"),
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("c"), LiteralNumberFactor(NumberToken(3.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("baz"),
                    LiteralArray(
                      List(
                        LiteralNumberFactor(NumberToken(1.0)),
                        LiteralNumberFactor(NumberToken(2.0)),
                        LiteralNumberFactor(NumberToken(3.0))
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("bal"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("a"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(WordToken("b"), LiteralNumberFactor(NumberToken(2.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("bay"),
                    LiteralProto(
                      WordToken("bat"),
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("c"), LiteralNumberFactor(NumberToken(3.0)))
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

    it("should bind a referenced inside array, object and proto if it is defined in scope") {
      bindAST("""conf foo : Foo {
            |     bar = 1
            |     bat = 2
            |     ban = "abc"
            |     baz = true
            |     bal = [bar, bat]
            |     bay = {
            |          a = bar
            |          b = bat
            |          c = ban
            |          d = baz
            |     }
            |     baf = bay { e = baz }
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1.0))),
                  ConfItem(WordToken("bat"), LiteralNumberFactor(NumberToken(2.0))),
                  ConfItem(WordToken("ban"), LiteralStringFactor(StringToken("abc"))),
                  ConfItem(WordToken("baz"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(
                    WordToken("bal"),
                    LiteralArray(
                      List(
                        LiteralNumberFactor(NumberToken(1.0)),
                        LiteralNumberFactor(NumberToken(2.0))
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("bay"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("a"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralNumberFactor(NumberToken(2.0))
                          ),
                          LiteralObjectItem(
                            WordToken("c"),
                            LiteralStringFactor(StringToken("abc"))
                          ),
                          LiteralObjectItem(WordToken("d"), LiteralBoolFactor(BoolToken(true)))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("baf"),
                    LiteralProto(
                      WordToken("bay"),
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("e"), LiteralBoolFactor(BoolToken(true)))
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

    it("should NOT bind a not defined reference in scope as array item") {
      bindAST("""conf foo : Foo {
            |     bar = [1, not_defined_reference]
            |}""".stripMargin) shouldEqual Left(
        ConfeeBinderError(
          Location(2, 16),
          "Reference error: 'not_defined_reference' is not defined"
        )
      )
    }

    it("should NOT bind a not defined reference in scope as object item") {
      bindAST("""conf foo : Foo {
            |     bar = { a = 1 b = not_defined_reference }
            |}""".stripMargin) shouldEqual Left(
        ConfeeBinderError(
          Location(2, 24),
          "Reference error: 'not_defined_reference' is not defined"
        )
      )
    }

    it("should NOT bind a not defined reference in scope as proto item") {
      bindAST("""conf foo : Foo {
            |     bar = { a = 1 b = 2 }
            |     bat = bar { c = not_defined_reference }
            |}""".stripMargin) shouldEqual Left(
        ConfeeBinderError(
          Location(3, 22),
          "Reference error: 'not_defined_reference' is not defined"
        )
      )
    }
  }

  describe("Binder on circular reference") {
    it("should handle self reference as error") {
      bindAST("""conf foo : Foo {
            |     bar = {
            |          a = 1
            |          b = 2
            |          c = bar
            |     }
            |}""".stripMargin) shouldEqual Left(
        ConfeeBinderError(
          Location(5, 15),
          "Reference error: 'bar' has a circular reference"
        )
      )
    }
  }

  describe("Binder on multi-level config") {
    it("should bind a reference to its closest relative in family hierarchy (2-level)") {
      bindAST("""conf foo : Foo {
            |     a = { x = 1 }
            |     x = 2
            |     b = x
            |     c = { x = 3 a = x }
            |     d = { a = x x = 4 }
            |     e = x
            |     f = { a = x }
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    WordToken("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("x"), LiteralNumberFactor(NumberToken(1.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(WordToken("x"), LiteralNumberFactor(NumberToken(2.0))),
                  ConfItem(WordToken("b"), LiteralNumberFactor(NumberToken(2.0))),
                  ConfItem(
                    WordToken("c"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(3.0))
                          ),
                          LiteralObjectItem(WordToken("a"), LiteralNumberFactor(NumberToken(3.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("d"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("a"),
                            LiteralNumberFactor(NumberToken(4.0))
                          ),
                          LiteralObjectItem(WordToken("x"), LiteralNumberFactor(NumberToken(4.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(WordToken("e"), LiteralNumberFactor(NumberToken(2.0))),
                  ConfItem(
                    WordToken("f"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("a"), LiteralNumberFactor(NumberToken(2.0)))
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

    it("should bind a reference to its closest relative in family hierarchy (2-level/reversed)") {
      bindAST("""conf foo : Foo {
            |     f = { a = x }
            |     e = x
            |     d = { a = x x = 4 }
            |     c = { x = 3 a = x }
            |     b = x
            |     x = 2
            |     a = { x = 1 }
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    WordToken("f"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("a"), LiteralNumberFactor(NumberToken(2.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(WordToken("e"), LiteralNumberFactor(NumberToken(2.0))),
                  ConfItem(
                    WordToken("d"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("a"),
                            LiteralNumberFactor(NumberToken(4.0))
                          ),
                          LiteralObjectItem(WordToken("x"), LiteralNumberFactor(NumberToken(4.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("c"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(3.0))
                          ),
                          LiteralObjectItem(WordToken("a"), LiteralNumberFactor(NumberToken(3.0)))
                        )
                      )
                    )
                  ),
                  ConfItem(WordToken("b"), LiteralNumberFactor(NumberToken(2.0))),
                  ConfItem(WordToken("x"), LiteralNumberFactor(NumberToken(2.0))),
                  ConfItem(
                    WordToken("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("x"), LiteralNumberFactor(NumberToken(1.0)))
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

    it("should bind a reference to its closest relative in family hierarchy (3-level)") {
      bindAST("""conf foo : Foo {
            |     x = 1
            |     a = { b = { x = 2 c = x } }
            |     c = { x = 3 b = { c = x } }
            |     d = { x = 4 a = x b = { c = x x = 5 } }
            |     e = { x = 6 b = { c = x } }
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("x"), LiteralNumberFactor(NumberToken(1.0))),
                  ConfItem(
                    WordToken("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("x"),
                                    LiteralNumberFactor(NumberToken(2.0))
                                  ),
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(2.0))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("c"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(3.0))
                          ),
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(3.0))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("d"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(4.0))
                          ),
                          LiteralObjectItem(
                            WordToken("a"),
                            LiteralNumberFactor(NumberToken(4.0))
                          ),
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(5.0))
                                  ),
                                  LiteralObjectItem(
                                    WordToken("x"),
                                    LiteralNumberFactor(NumberToken(5.0))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("e"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(6.0))
                          ),
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(6.0))
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

    it("should bind a reference to its closest relative in family hierarchy (3-level/reversed)") {
      bindAST("""conf foo : Foo {
            |     e = { x = 6 b = { c = x } }
            |     d = { x = 4 a = x b = { c = x x = 5 } }
            |     c = { x = 3 b = { c = x } }
            |     a = { b = { x = 2 c = x } }
            |     x = 1
            |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    WordToken("e"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(6.0))
                          ),
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(6.0))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("d"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(4.0))
                          ),
                          LiteralObjectItem(
                            WordToken("a"),
                            LiteralNumberFactor(NumberToken(4.0))
                          ),
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(5.0))
                                  ),
                                  LiteralObjectItem(
                                    WordToken("x"),
                                    LiteralNumberFactor(NumberToken(5.0))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("c"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("x"),
                            LiteralNumberFactor(NumberToken(3.0))
                          ),
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(3.0))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("b"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("x"),
                                    LiteralNumberFactor(NumberToken(2.0))
                                  ),
                                  LiteralObjectItem(
                                    WordToken("c"),
                                    LiteralNumberFactor(NumberToken(2.0))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(WordToken("x"), LiteralNumberFactor(NumberToken(1.0)))
                )
              )
            )
          )
        )
      )
    }

  }

  describe("Binder on multiple conf") {
    it("should bind a reference to a conf inside another conf") {
      bindAST("""
            |conf foo : Foo {
            |     a = 1
            |     b = 2
            |}
            |
            |conf bar : Bar {
            |     a = foo
            |}
            |""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("a"), LiteralNumberFactor(NumberToken(1.0))),
                  ConfItem(WordToken("b"), LiteralNumberFactor(NumberToken(2.0)))
                )
              )
            ),
            ConfStmt(
              WordToken("bar"),
              TypeDef(Left(NameToken("Bar")), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    WordToken("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("a"), LiteralNumberFactor(NumberToken(1.0))),
                          LiteralObjectItem(WordToken("b"), LiteralNumberFactor(NumberToken(2.0)))
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

  def bindAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens <- ConfeeLexer(input)
      parsed <- ConfeeParser(tokens)
      bound  <- ConfeeBinder(parsed)
    } yield bound
  }
}
