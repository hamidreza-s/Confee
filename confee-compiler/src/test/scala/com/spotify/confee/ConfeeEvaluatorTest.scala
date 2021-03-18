package com.spotify.confee

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeEvaluatorTest extends AnyFunSpec with Matchers {

  describe("Evaluator on literal bool (bitwise)") {
    it("should evaluate bitwise operator on literal bool group") {
      evaluateAST("""conf foo : Foo {
                           |     bar = not false
                           |     bat = true and false
                           |     ban = true or false
                           |     bal = true xor false
                           |     baz = not (true and false)
                           |     bay = true and (false or true)
                           |     bax = false or (false and true)
                           |     baw = true xor (false xor true)
                           |     bav = false and (not true)
                           |     bau = (true and false) xor (true or false)
                           |     bas = (true or false) and not true or false
                           |     baq = true and false or true and false xor true
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("bar"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(ConfItemKey("bat"), LiteralBoolFactor(BoolToken(false))),
                  ConfItem(ConfItemKey("ban"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(ConfItemKey("bal"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(ConfItemKey("baz"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(ConfItemKey("bay"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(ConfItemKey("bax"), LiteralBoolFactor(BoolToken(false))),
                  ConfItem(ConfItemKey("baw"), LiteralBoolFactor(BoolToken(false))),
                  ConfItem(ConfItemKey("bav"), LiteralBoolFactor(BoolToken(false))),
                  ConfItem(ConfItemKey("bau"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(ConfItemKey("bas"), LiteralBoolFactor(BoolToken(false))),
                  ConfItem(ConfItemKey("baq"), LiteralBoolFactor(BoolToken(true)))
                )
              )
            )
          )
        )
      )
    }

    it("should fail when a literal word presents and has not been referenced in binder step") {
      evaluateAST("""conf foo : Foo {
                           |     bar = true
                           |     bat = false and bar
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError(
          Location(3, 22),
          "Literal Bool Word must have been referenced in binder step"
        )
      )

      evaluateAST("""conf foo : Foo {
                           |     bar = true
                           |     bat = bar or true
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError(
          Location(3, 12),
          "Literal Bool Word must have been referenced in binder step"
        )
      )
    }
  }

  describe("Evaluator on literal string") {
    it("should evaluate concat operator on literal string group") {
      evaluateAST("""conf foo : Foo {
                           |     bar = "a" + "b"
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("bar"), LiteralStringFactor(StringToken("ab")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate remove operator on literal string group") {
      evaluateAST("""conf foo : Foo {
                           |     bar = "a" - "b"
                           |     bat = "abc" - "b"
                           |     ban = "abcabc" - "abc"
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("bar"), LiteralStringFactor(StringToken("a"))),
                  ConfItem(ConfItemKey("bat"), LiteralStringFactor(StringToken("ac"))),
                  ConfItem(ConfItemKey("ban"), LiteralStringFactor(StringToken("abc")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate concat operator on literal string group in group (recursive)") {
      evaluateAST("""conf foo : Foo {
                           |     bar = "a" + "bc" + "d"
                           |     bat = ("a" + "bc") + "d"
                           |     ban = "a" + ("bc" + "d")
                           |     bal = ("a" + "bc" + "d")
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("bar"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(ConfItemKey("bat"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(ConfItemKey("ban"), LiteralStringFactor(StringToken("abcd"))),
                  ConfItem(ConfItemKey("bal"), LiteralStringFactor(StringToken("abcd")))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate remove operator on literal string group in group (recursive)") {
      // NOTE: by default when there is no grouping string operators are right-associative
      evaluateAST("""conf foo : Foo {
                           |     bar = "abc" - "efg" - "abc"
                           |     bat = "ab" - "bc" - "c"
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("bar"), LiteralStringFactor(StringToken("abc"))),
                  ConfItem(ConfItemKey("bat"), LiteralStringFactor(StringToken("a")))
                )
              )
            )
          )
        )
      )
    }

    it("should fail when a literal word presents and has not been referenced in binder step") {
      evaluateAST("""conf foo : Foo {
                           |     bar = "abc"
                           |     bat = "abc" + bar
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError(
          Location(3, 20),
          "Literal String Word must have been referenced in binder step"
        )
      )

      evaluateAST("""conf foo : Foo {
                           |     bar = "abc"
                           |     bat = bar + "abc"
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError(
          Location(3, 12),
          "Literal String Word must have been referenced in binder step"
        )
      )
    }

  }

  describe("Evaluator on literal number") {
    it("should evaluate arithmetic operator on literal number group") {
      evaluateAST("""conf foo : Foo {
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("bar"), LiteralNumberFactor(NumberToken(12))),
                  ConfItem(ConfItemKey("bat"), LiteralNumberFactor(NumberToken(8))),
                  ConfItem(ConfItemKey("ban"), LiteralNumberFactor(NumberToken(20))),
                  ConfItem(ConfItemKey("bal"), LiteralNumberFactor(NumberToken(5))),
                  ConfItem(ConfItemKey("baz"), LiteralNumberFactor(NumberToken(0)))
                )
              )
            )
          )
        )
      )
    }

    it("should evaluate arithmetic operator on literal number group in group (recursive)") {
      // NOTE: by default when there is no grouping arithmetic operators are right-associative
      evaluateAST("""conf foo : Foo {
                           |     bar = 1 + (2 + 3) + 4
                           |     bat = 2 * (3 + 2)
                           |     ban = (4 - 2) * ((3 + 3) / (4 - 2)) + 5
                           |     bal = ((1 + 1) * (1 - 1)) + 1 + (5 % 2)
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("bar"), LiteralNumberFactor(NumberToken(10))),
                  ConfItem(ConfItemKey("bat"), LiteralNumberFactor(NumberToken(10))),
                  ConfItem(ConfItemKey("ban"), LiteralNumberFactor(NumberToken(16))),
                  ConfItem(ConfItemKey("bal"), LiteralNumberFactor(NumberToken(2)))
                )
              )
            )
          )
        )
      )
    }

    it("should fail when a literal word presents and has not been referenced in binder step") {
      evaluateAST("""conf foo : Foo {
                           |     bar = 123
                           |     bat = 123 + bar
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError(
          Location(3, 18),
          "Literal Number Word must have been referenced in binder step"
        )
      )

      evaluateAST("""conf foo : Foo {
                           |     bar = 123
                           |     bat = bar + 123
                           |}""".stripMargin) shouldEqual Left(
        ConfeeEvaluatorError(
          Location(3, 12),
          "Literal Number Word must have been referenced in binder step"
        )
      )
    }
  }

  describe("Evaluator on literal array") {
    it("should evaluate literal array of evaluated bool, string, number, array, object & proto") {
      evaluateAST("""conf foo : Foo {
          |     bar = [not false, false and true, false or true]
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("bar"),
                    LiteralArray(
                      List(
                        LiteralBoolFactor(BoolToken(true)),
                        LiteralBoolFactor(BoolToken(false)),
                        LiteralBoolFactor(BoolToken(true))
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("bat"),
                    LiteralArray(
                      List(
                        LiteralStringFactor(StringToken("ab")),
                        LiteralStringFactor(StringToken("ef")),
                        LiteralStringFactor(StringToken(""))
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("ban"),
                    LiteralArray(
                      List(
                        LiteralStringFactor(StringToken("abc")),
                        LiteralStringFactor(StringToken("def")),
                        LiteralStringFactor(StringToken("ghi"))
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("bal"),
                    LiteralArray(
                      List(
                        LiteralNumberFactor(NumberToken(1)),
                        LiteralNumberFactor(NumberToken(2)),
                        LiteralNumberFactor(NumberToken(13))
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("baz"),
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
                    ConfItemKey("bay"),
                    LiteralArray(
                      List(
                        LiteralObject(
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("bax"),
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
                                LiteralObjectItemKey("baw"),
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
                    ConfItemKey("bav"),
                    LiteralArray(
                      List(
                        LiteralProto(
                          LiteralProtoKey("bau"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("bas"),
                                LiteralStringFactor(StringToken("ab"))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("baq"),
                                LiteralNumberFactor(NumberToken(2))
                              )
                            )
                          )
                        ),
                        LiteralProto(
                          LiteralProtoKey("bau"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("bas"),
                                LiteralStringFactor(StringToken("ab"))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("baq"),
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
      evaluateAST("""conf foo : Foo {
                           |     bar = {
                           |          ban = not ((true and true) xor not false)
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("bar"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("ban"),
                            LiteralBoolFactor(BoolToken(true))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("bat"),
                            LiteralStringFactor(StringToken("abc"))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("bal"),
                            LiteralNumberFactor(NumberToken(21))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("baz"),
                            LiteralArray(
                              List(
                                LiteralStringFactor(StringToken("abcd")),
                                LiteralStringFactor(StringToken("efgh"))
                              )
                            )
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("bay"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("bax"),
                                    LiteralNumberFactor(NumberToken(3))
                                  ),
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("baw"),
                                    LiteralStringFactor(StringToken("ab"))
                                  )
                                )
                              )
                            )
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("bav"),
                            LiteralArray(
                              List(
                                LiteralProto(
                                  LiteralProtoKey("bau"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("bas"),
                                        LiteralStringFactor(StringToken("ab"))
                                      ),
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("baq"),
                                        LiteralNumberFactor(NumberToken(2))
                                      )
                                    )
                                  )
                                ),
                                LiteralProto(
                                  LiteralProtoKey("bau"),
                                  LiteralObjectItems(
                                    List(
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("bas"),
                                        LiteralStringFactor(StringToken("ab"))
                                      ),
                                      LiteralObjectItem(
                                        LiteralObjectItemKey("baq"),
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
      evaluateAST("""conf foo : Foo {
                           |     bar = ban { bat = true and true bal = "a" + "b"}
                           |     baw = [
                           |          ban { bat = true and true bal = "a" + "b" },
                           |          baz { bay = "abc" - "b" bax = (1 + 2) * 3 }
                           |     ]
                           |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("bar"),
                    LiteralProto(
                      LiteralProtoKey("ban"),
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("bat"),
                            LiteralBoolFactor(BoolToken(true))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("bal"),
                            LiteralStringFactor(StringToken("ab"))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("baw"),
                    LiteralArray(
                      List(
                        LiteralProto(
                          LiteralProtoKey("ban"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("bat"),
                                LiteralBoolFactor(BoolToken(true))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("bal"),
                                LiteralStringFactor(StringToken("ab"))
                              )
                            )
                          )
                        ),
                        LiteralProto(
                          LiteralProtoKey("baz"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("bay"),
                                LiteralStringFactor(StringToken("ac"))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("bax"),
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

  def evaluateAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens    <- ConfeeLexer(input)
      parsed    <- ConfeeParser(tokens)
      linked    <- ConfeeLinker(parsed)
      validated <- ConfeeValidator(linked)
      evaluated <- ConfeeEvaluator(validated)
    } yield evaluated
  }
}
