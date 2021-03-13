package com.spotify.confee

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeConstructorTest extends AnyFunSpec with Matchers {

  describe("Constructor on literal proto") {
    it("should construct an object from a proto both at the same level") {
      constructedAST("""conf foo : Foo {
          |     a = {
          |          x = true
          |          y = 1.0
          |          z = "Z"
          |     }
          |     b = a {
          |          x = false
          |          y = 2.0
          |     }
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
                          LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(true))),
                          LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(1.0))),
                          LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z")))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(false))),
                          LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(2.0))),
                          LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z")))
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

    it("should construct an object from a proto inside an array") {
      constructedAST("""conf foo : Foo {
                       |     a = {
                       |          x = true
                       |          y = 1.0
                       |          z = "Z"
                       |     }
                       |     b = [
                       |          a {
                       |               x = false
                       |               y = 2.0
                       |          },
                       |          a {
                       |               x = true
                       |               y = 3.0
                       |          }
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
                    WordToken("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(true))),
                          LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(1.0))),
                          LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z")))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("b"),
                    LiteralArray(
                      List(
                        LiteralObject(
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                WordToken("x"),
                                LiteralBoolFactor(BoolToken(false))
                              ),
                              LiteralObjectItem(
                                WordToken("y"),
                                LiteralNumberFactor(NumberToken(2.0))
                              ),
                              LiteralObjectItem(
                                WordToken("z"),
                                LiteralStringFactor(StringToken("Z"))
                              )
                            )
                          )
                        ),
                        LiteralObject(
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(true))),
                              LiteralObjectItem(
                                WordToken("y"),
                                LiteralNumberFactor(NumberToken(3.0))
                              ),
                              LiteralObjectItem(
                                WordToken("z"),
                                LiteralStringFactor(StringToken("Z"))
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

    it("should construct an object from a proto both at same level with new item") {
      constructedAST("""conf foo : Foo {
          |     a = {
          |          x = true
          |          y = 1.0
          |          z = "Z"
          |     }
          |     b = a {
          |          n = "N"
          |          x = false
          |          y = 2.0
          |     }
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
                          LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(true))),
                          LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(1.0))),
                          LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z")))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(false))),
                          LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(2.0))),
                          LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z"))),
                          LiteralObjectItem(WordToken("n"), LiteralStringFactor(StringToken("N")))
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

    it("should construct objects from a proto having another proto inside") {
      constructedAST("""conf foo : Foo {
          |     a = {
          |          x = true
          |          y = 1.0
          |          z = "Z"
          |     }
          |     b = a {
          |          y = 2.0
          |          m = a {
          |               y = 3.0
          |          }
          |     }
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
                          LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(true))),
                          LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(1.0))),
                          LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z")))
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(true))),
                          LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(2.0))),
                          LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z"))),
                          LiteralObjectItem(
                            WordToken("m"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    WordToken("x"),
                                    LiteralBoolFactor(BoolToken(true))
                                  ),
                                  LiteralObjectItem(
                                    WordToken("y"),
                                    LiteralNumberFactor(NumberToken(3.0))
                                  ),
                                  LiteralObjectItem(
                                    WordToken("z"),
                                    LiteralStringFactor(StringToken("Z"))
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

    it("should construct objects from a proto having reference which needs to be bound") {
      constructedAST("""conf foo : Foo {
          |     base = 1
          |     a = {
          |          index = base
          |     }
          |     b = a {
          |         index = base + 1
          |     }
          |}""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(Left(NameToken("Foo")), isList = false),
              ConfItems(
                List(
                  ConfItem(WordToken("base"), LiteralNumberFactor(NumberToken(1.0))),
                  ConfItem(
                    WordToken("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("index"),
                            LiteralNumberFactor(NumberToken(1.0))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    WordToken("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            WordToken("index"),
                            LiteralNumberFactor(NumberToken(2.0))
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

  def constructedAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens      <- ConfeeLexer(input)
      parsed      <- ConfeeParser(tokens)
      linked      <- ConfeeLinker(parsed)
      bound       <- ConfeeBinder(linked)
      evaluated   <- ConfeeEvaluator(bound)
      constructed <- ConfeeConstructor(evaluated)
    } yield constructed
  }

}
