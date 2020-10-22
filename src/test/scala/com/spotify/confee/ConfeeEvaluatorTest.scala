package com.spotify.confee

import org.scalatest.{FunSpec, Matchers}

class ConfeeEvaluatorTest extends FunSpec with Matchers {

  // TODO: add tests for booleans once it is implemented

  describe("Evaluator on literal string") {
    it("should evaluate concat operator on literal string group") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "a" + "b"
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("ab")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate remove operator on literal string group") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "a" - "b"
                           |     bat = "abc" - "b"
                           |     ban = "abcabc" - "abc"
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("a"))),
                  ConfItem(WordToken("bat"), LiteralStringFactor(StringToken("ac"))),
                  ConfItem(WordToken("ban"), LiteralStringFactor(StringToken("abc")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate concat operator on literal string group in group (recursive)") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "a" + "bc" + "d"
                           |     bat = ("a" + "bc") + "d"
                           |     ban = "a" + ("bc" + "d")
                           |     bal = ("a" + "bc" + "d")
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(WordToken("bat"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(WordToken("ban"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(WordToken("bal"), LiteralStringFactor(StringToken("abcd")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate remove operator on literal string group in group (recursive)") {
      // NOTE: by default when there is no grouping string operators are right-associative
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "abc" - "efg" - "abc"
                           |     bat = "ab" - "bc" - "c"
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralStringFactor(StringToken("abc"))),
                  ConfItem(WordToken("bat"), LiteralStringFactor(StringToken("a")))
                )
              )
            )
          )
        )
      )
    }

    it("should fail when a literal word presents and has not been referenced in binder step") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "abc"
                           |     bat = "abc" + bar
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal String Word must have been referenced in binder step")
      )

      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = "abc"
                           |     bat = bar + "abc"
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal String Word must have been referenced in binder step")
      )
    }

  }

  describe("Evaluator on literal number") {
    it("should evaluate arithmetic operator on literal number group") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 10 + 2
                           |     bat = 10 - 2
                           |     ban = 10 * 2
                           |     bal = 10 / 2
                           |     baz = 10 % 2
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralNumberFactor(NumberToken(12))),
                  ConfItem(WordToken("bat"), LiteralNumberFactor(NumberToken(8))),
                  ConfItem(WordToken("ban"), LiteralNumberFactor(NumberToken(20))),
                  ConfItem(WordToken("bal"), LiteralNumberFactor(NumberToken(5))),
                  ConfItem(WordToken("baz"), LiteralNumberFactor(NumberToken(0)))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate arithmetic operator on literal number group in group (recursive)") {
      // NOTE: by default when there is no grouping arithmetic operators are right-associative
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 1 + (2 + 3) + 4
                           |     bat = 2 * (3 + 2)
                           |     ban = (4 - 2) * ((3 + 3) / (4 - 2)) + 5
                           |     bal = ((1 + 1) * (1 - 1)) + 1 + (5 % 2)
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("bar"), LiteralNumberFactor(NumberToken(10))),
                  ConfItem(WordToken("bat"), LiteralNumberFactor(NumberToken(10))),
                  ConfItem(WordToken("ban"), LiteralNumberFactor(NumberToken(16))),
                  ConfItem(WordToken("bal"), LiteralNumberFactor(NumberToken(2)))
                )
              )
            )
          )
        )
      )
    }

    it("should fail when a literal word presents and has not been referenced in binder step") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 123
                           |     bat = 123 + bar
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal Number Word must have been referenced in binder step")
      )

      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = 123
                           |     bat = bar + 123
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError("Literal Number Word must have been referenced in binder step")
      )
    }
  }

  describe("Evaluator on literal array") {
    it("should evaluate literal array of evaluated bool, string, number, array, object & proto") {
      assertEvaluatedAST("""conf foo : Foo {
          |     bar = [true, false, true]
          |     bat = ["abc" - "c", "def" - "d", "ghi" - "ghi"]
          |     ban = ["a" + "b" + "c", "d" + "ef", "gh" + "i"]
          |     bal = [1, 1 + 1, 1 + (2 * 3) + (4 + 5 - (6 / 2))]
          |     baz = [[1 + 1, 2 + 2], [3 + 3, 4 + 4]]
          |     bay = [{bax = "a" + "b"}, {baw = 1 + 1}]
          |     bav = [
          |          bau { bas = "a" + "b" baq = 1 + 1 },
          |          bau { bas = "abc" - "c" baq = 1 * (2 + 3) }
          |     ]
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
                    LiteralArray(List(LiteralBoolTrue(), LiteralBoolFalse(), LiteralBoolTrue()))
                  ),
                  ConfItem(
                    WordToken("bat"),
                    LiteralArray(
                      List(
                        LiteralStringFactor(StringToken("ab")),
                        LiteralStringFactor(StringToken("ef")),
                        LiteralStringFactor(StringToken(""))
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("ban"),
                    LiteralArray(
                      List(
                        LiteralStringFactor(StringToken("abc")),
                        LiteralStringFactor(StringToken("def")),
                        LiteralStringFactor(StringToken("ghi"))
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("bal"),
                    LiteralArray(
                      List(
                        LiteralNumberFactor(NumberToken(1)),
                        LiteralNumberFactor(NumberToken(2)),
                        LiteralNumberFactor(NumberToken(13))
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("baz"),
                    LiteralArray(
                      List(
                        LiteralArray(
                          List(
                            LiteralNumberFactor(NumberToken(2)),
                            LiteralNumberFactor(NumberToken(4))
                          )
                        ),
                        LiteralArray(
                          List(
                            LiteralNumberFactor(NumberToken(6.0)),
                            LiteralNumberFactor(NumberToken(8.0))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("bay"),
                    LiteralArray(
                      List(
                        LiteralObject(
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                WordToken("bax"),
                                LiteralStringFactor(
                                  StringToken("ab")
                                )
                              )
                            )
                          )
                        ),
                        LiteralObject(
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                WordToken("baw"),
                                LiteralNumberFactor(
                                  NumberToken(2)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("bav"),
                    LiteralArray(
                      List(
                        LiteralProto(
                          WordToken("bau"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                WordToken("bas"),
                                LiteralStringFactor(StringToken("ab"))
                              ),
                              LiteralObjectItem(
                                WordToken("baq"),
                                LiteralNumberFactor(NumberToken(2))
                              )
                            )
                          )
                        ),
                        LiteralProto(
                          WordToken("bau"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                WordToken("bas"),
                                LiteralStringFactor(StringToken("ab"))
                              ),
                              LiteralObjectItem(
                                WordToken("baq"),
                                LiteralNumberFactor(NumberToken(5))
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

  describe("Evaluator on literal object") {
    it("should evaluate literal object of evaluated bool, string, number, array, object & proto") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = {
                           |          ban = true
                           |          bat = "a" + "bcde" - "de"
                           |          bal = (1 + 2) * (3 + 4)
                           |          baz = ["ab" + "cd", "ef" + "gh"]
                           |          bay = {
                           |               bax = 1 + 2
                           |               baw = "a" + "b"
                           |          }
                           |          bav = [
                           |               bau { bas = "a" + "b" baq = 1 + 1 },
                           |               bau { bas = "abc" - "c" baq = 1 * (2 + 3) }
                           |          ]
                           |     }
                           |}
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
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("ban"), LiteralBoolTrue()),
                          LiteralObjectItem(
                            WordToken("bat"),
                            LiteralStringFactor(StringToken("abc"))
                          ),
                          LiteralObjectItem(WordToken("bal"), LiteralNumberFactor(NumberToken(21))),
                          LiteralObjectItem(
                            WordToken("baz"),
                            LiteralArray(
                              List(
                                LiteralStringFactor(StringToken("abcd")),
                                LiteralStringFactor(StringToken("efgh"))
                              )
                            )
                          ),
                          LiteralObjectItem(
                            WordToken("bay"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("bax"),
                                    LiteralNumberFactor(NumberToken(3))
                                  ),
                                  LiteralObjectItem(
                                    WordToken("baw"),
                                    LiteralStringFactor(StringToken("ab"))
                                  )
                                )
                              )
                            )
                          ),
                          LiteralObjectItem(
                            WordToken("bav"),
                            LiteralArray(
                              List(
                                LiteralProto(
                                  WordToken("bau"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        WordToken("bas"),
                                        LiteralStringFactor(StringToken("ab"))
                                      ),
                                      LiteralObjectItem(
                                        WordToken("baq"),
                                        LiteralNumberFactor(NumberToken(2))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  WordToken("bau"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        WordToken("bas"),
                                        LiteralStringFactor(StringToken("ab"))
                                      ),
                                      LiteralObjectItem(
                                        WordToken("baq"),
                                        LiteralNumberFactor(NumberToken(5))
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

  describe("Evaluator on literal proto") {
    it("should evaluate literal proto of evaluated bool, string, number, array & nested object") {
      assertEvaluatedAST("""conf foo : Foo {
                           |     bar = [
                           |          ban { bat = true bal = "a" + "b" },
                           |          baz { bay = "abc" - "b" bax = (1 + 2) * 3 }
                           |     ]
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
                        LiteralProto(
                          WordToken("ban"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                WordToken("bat"),
                                LiteralBoolTrue()
                              ),
                              LiteralObjectItem(
                                WordToken("bal"),
                                LiteralStringFactor(StringToken("ab"))
                              )
                            )
                          )
                        ),
                        LiteralProto(
                          WordToken("baz"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                WordToken("bay"),
                                LiteralStringFactor(StringToken("ac"))
                              ),
                              LiteralObjectItem(
                                WordToken("bax"),
                                LiteralNumberFactor(NumberToken(9))
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

  def assertEvaluatedAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens    <- ConfeeLexer(input).right
      parsed    <- ConfeeParser(tokens).right
      evaluated <- ConfeeEvaluator(parsed)
    } yield evaluated
  }
}
