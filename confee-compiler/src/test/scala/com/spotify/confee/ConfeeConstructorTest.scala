package com.spotify.confee

import com.spotify.confee.ConfeeIndexer.IndexRow
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeConstructorTest extends AnyFunSpec with Matchers {

  describe("Indexer on config objects") {
    it("should index config objects and its children items WITHOUT reference") {
      indexAST("""conf foo : Foo {
                 |     a = true
                 |     b = "abc"
                 |     c = 1
                 |     d = [1]
                 |     e = {bar = 1 bat = {ban = 2}}
                 |     f = g {bar = 1 bat = {ban = 2}}
          |}""".stripMargin) shouldEqual List(
        IndexRow(
          WordToken("e"),
          List(WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1.0))),
                LiteralObjectItem(
                  WordToken("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(WordToken("ban"), LiteralNumberFactor(NumberToken(2.0)))
                      )
                    )
                  )
                )
              )
            )
          ),
          hasReference = false
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("e"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralNumberFactor(NumberToken(2.0))))
            )
          ),
          hasReference = false
        )
      )
    }

    it("should index config objects and its children items WITH reference") {
      indexAST("""conf foo : Foo {
          |     a = false and ref1
          |     b = "abc" + ref2
          |     c = 1 + ref3
          |     d = [ref4]
          |     e = {bar = 1 bat = {ban = ref5}}
          |     f = g {bar = 1 bat = {ban = ref6}}
          |}""".stripMargin) shouldEqual List(
        IndexRow(
          WordToken("e"),
          List(WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1.0))),
                LiteralObjectItem(
                  WordToken("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref5"))))
                    )
                  )
                )
              )
            )
          ),
          hasReference = true
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("e"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref5"))))
            )
          ),
          hasReference = true
        )
      )
    }
  }

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
                              LiteralObjectItem(WordToken("x"), LiteralBoolFactor(BoolToken(false))),
                              LiteralObjectItem(
                                WordToken("y"),
                                LiteralNumberFactor(NumberToken(2.0))
                              ),
                              LiteralObjectItem(WordToken("z"), LiteralStringFactor(StringToken("Z")))
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

  def indexAST(input: String): List[IndexRow[LiteralObject]] = {
    (for {
      tokens <- ConfeeLexer(input).right
      parsed <- ConfeeParser(tokens).right
    } yield parsed) match {
      case Right(Grammar(stmts: List[Stmt])) => ConfeeConstructor.indexObjects(stmts)
      case error                             => fail(error.toString)
    }
  }

  def constructedAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens      <- ConfeeLexer(input).right
      parsed      <- ConfeeParser(tokens).right
      linked      <- ConfeeLinker(parsed).right
      bound       <- ConfeeBinder(linked).right
      evaluated   <- ConfeeEvaluator(bound).right
      constructed <- ConfeeConstructor(evaluated).right
    } yield constructed
  }

}
