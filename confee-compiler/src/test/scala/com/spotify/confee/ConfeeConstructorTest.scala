package com.spotify.confee

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeConstructorTest extends AnyFunSpec with Matchers {

  describe("Constructor on literal proto") {
    it("should construct an object from a proto both at the same level") {
      constructAST("""conf foo : Foo {
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("x"),
                            LiteralBoolFactor(BoolToken(true))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("y"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("z"),
                            LiteralStringFactor(StringToken("Z"))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("x"),
                            LiteralBoolFactor(BoolToken(false))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("y"),
                            LiteralNumberFactor(NumberToken(2.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("z"),
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
    }

    it("should construct an object from a proto inside an array") {
      constructAST("""conf foo : Foo {
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("x"),
                            LiteralBoolFactor(BoolToken(true))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("y"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("z"),
                            LiteralStringFactor(StringToken("Z"))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("b"),
                    LiteralArray(
                      List(
                        LiteralObject(
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("x"),
                                LiteralBoolFactor(BoolToken(false))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("y"),
                                LiteralNumberFactor(NumberToken(2.0))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("z"),
                                LiteralStringFactor(StringToken("Z"))
                              )
                            )
                          )
                        ),
                        LiteralObject(
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("x"),
                                LiteralBoolFactor(BoolToken(true))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("y"),
                                LiteralNumberFactor(NumberToken(3.0))
                              ),
                              LiteralObjectItem(
                                LiteralObjectItemKey("z"),
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
      constructAST("""conf foo : Foo {
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("x"),
                            LiteralBoolFactor(BoolToken(true))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("y"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("z"),
                            LiteralStringFactor(StringToken("Z"))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("x"),
                            LiteralBoolFactor(BoolToken(false))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("y"),
                            LiteralNumberFactor(NumberToken(2.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("z"),
                            LiteralStringFactor(StringToken("Z"))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("n"),
                            LiteralStringFactor(StringToken("N"))
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

    it("should construct objects from a proto having another proto inside") {
      constructAST("""conf foo : Foo {
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("x"),
                            LiteralBoolFactor(BoolToken(true))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("y"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("z"),
                            LiteralStringFactor(StringToken("Z"))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("x"),
                            LiteralBoolFactor(BoolToken(true))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("y"),
                            LiteralNumberFactor(NumberToken(2.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("z"),
                            LiteralStringFactor(StringToken("Z"))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("m"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("x"),
                                    LiteralBoolFactor(BoolToken(true))
                                  ),
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("y"),
                                    LiteralNumberFactor(NumberToken(3.0))
                                  ),
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("z"),
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
      constructAST("""conf foo : Foo {
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
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("base"), LiteralNumberFactor(NumberToken(1.0))),
                  ConfItem(
                    ConfItemKey("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("index"),
                            LiteralNumberFactor(NumberToken(1.0))
                          )
                        )
                      )
                    )
                  ),
                  ConfItem(
                    ConfItemKey("b"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("index"),
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

    it("should construct object from a proto having reference to a conf") {
      constructAST("""
                |conf foo : Foo {
                |     a = 1
                |     b = 2
                |}
                |
                |conf bar : Bar {
                |     a = foo { c = 3 }
                |}
                |""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("a"), LiteralNumberFactor(NumberToken(1.0))),
                  ConfItem(ConfItemKey("b"), LiteralNumberFactor(NumberToken(2.0)))
                )
              )
            ),
            ConfStmt(
              WordToken("bar"),
              TypeDef(NameToken("Bar"), isList = false),
              ConfItems(
                List(
                  ConfItem(
                    ConfItemKey("a"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("a"),
                            LiteralNumberFactor(NumberToken(1.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("b"),
                            LiteralNumberFactor(NumberToken(2.0))
                          ),
                          LiteralObjectItem(
                            LiteralObjectItemKey("c"),
                            LiteralNumberFactor(NumberToken(3.0))
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

  def constructAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens      <- ConfeeLexer(input)
      parsed      <- ConfeeParser(tokens)
      linked      <- ConfeeLinker(parsed)
      validated   <- ConfeeValidator(linked)
      bound       <- ConfeeBinder(validated)
      evaluated   <- ConfeeEvaluator(bound)
      constructed <- ConfeeConstructor(evaluated)
    } yield constructed
  }

}
