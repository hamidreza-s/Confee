package com.spotify.confee

import com.spotify.confee.ConfeeIndexer._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeIndexerTest extends AnyFunSpec with Matchers {

  describe("Indexer on statements") {
    it("should index config items and its object or proto children WITHOUT reference") {
      indexStmts("""conf foo : Foo {
          |     a = true
          |     b = "abc"
          |     c = 1
          |     d = [1]
          |     e = {bar = 1 bat = {ban = 2}}
          |     f = g {bar = 1 bat = {ban = 2}}
          |}""".stripMargin) shouldEqual List(
        IndexRow(
          WordToken("foo"),
          List(),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(WordToken("a"), LiteralBoolFactor(BoolToken(true))),
                LiteralObjectItem(WordToken("b"), LiteralStringFactor(StringToken("abc"))),
                LiteralObjectItem(WordToken("c"), LiteralNumberFactor(NumberToken(1.0))),
                LiteralObjectItem(
                  WordToken("d"),
                  LiteralArray(List(LiteralNumberFactor(NumberToken(1.0))))
                ),
                LiteralObjectItem(
                  WordToken("e"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1.0))),
                        LiteralObjectItem(
                          WordToken("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(
                                  WordToken("ban"),
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
                LiteralObjectItem(
                  WordToken("f"),
                  LiteralProto(
                    WordToken("g"),
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1.0))),
                        LiteralObjectItem(
                          WordToken("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(
                                  WordToken("ban"),
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
          ),
          ObjectType,
          hasReference = false,
          isTopLevel = true
        ),
        IndexRow(
          WordToken("a"),
          List(WordToken("foo")),
          LiteralBoolFactor(BoolToken(true)),
          BoolType,
          hasReference = false
        ),
        IndexRow(
          WordToken("b"),
          List(WordToken("foo")),
          LiteralStringFactor(StringToken("abc")),
          StringType,
          hasReference = false
        ),
        IndexRow(
          WordToken("c"),
          List(WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          NumberType,
          hasReference = false
        ),
        IndexRow(
          WordToken("d"),
          List(WordToken("foo")),
          LiteralArray(List(LiteralNumberFactor(NumberToken(1.0)))),
          ArrayType,
          hasReference = false
        ),
        IndexRow(
          WordToken("e"),
          List(WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  WordToken("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(LiteralObjectItem(WordToken("ban"), LiteralNumberFactor(NumberToken(2))))
                    )
                  )
                )
              )
            )
          ),
          ObjectType,
          hasReference = false
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("e"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          NumberType,
          hasReference = false
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("e"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralNumberFactor(NumberToken(2))))
            )
          ),
          ObjectType,
          hasReference = false
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("e"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(2)),
          NumberType,
          hasReference = false
        ),
        IndexRow(
          WordToken("f"),
          List(WordToken("foo")),
          LiteralProto(
            WordToken("g"),
            LiteralObjectItems(
              List(
                LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  WordToken("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(LiteralObjectItem(WordToken("ban"), LiteralNumberFactor(NumberToken(2))))
                    )
                  )
                )
              )
            )
          ),
          ProtoType,
          hasReference = false
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("f"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          NumberType,
          hasReference = false
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("f"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralNumberFactor(NumberToken(2))))
            )
          ),
          ObjectType,
          hasReference = false
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("f"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(2)),
          NumberType,
          hasReference = false
        )
      )
    }

    it("should index config items and its object or proto children WITH reference") {
      indexStmts("""conf foo : Foo {
          |     a = false and ref1
          |     b = "abc" + ref2
          |     c = 1 + ref3
          |     d = [ref4]
          |     e = {bar = 1 bat = {ban = ref5}}
          |     f = g {bar = 1 bat = {ban = ref6}}
          |}""".stripMargin) shouldEqual List(
        IndexRow(
          WordToken("foo"),
          List(),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(
                  WordToken("a"),
                  LiteralBoolGroup(
                    LiteralBoolOperatorAnd(),
                    LiteralBoolFactor(BoolToken(false)),
                    LiteralBoolWord(WordToken("ref1"))
                  )
                ),
                LiteralObjectItem(
                  WordToken("b"),
                  LiteralStringGroup(
                    LiteralStringOperatorConcat(),
                    LiteralStringFactor(StringToken("abc")),
                    LiteralStringWord(WordToken("ref2"))
                  )
                ),
                LiteralObjectItem(
                  WordToken("c"),
                  LiteralNumberGroup(
                    LiteralNumberOperatorAdd(),
                    LiteralNumberFactor(NumberToken(1.0)),
                    LiteralNumberWord(WordToken("ref3"))
                  )
                ),
                LiteralObjectItem(
                  WordToken("d"),
                  LiteralArray(List(LiteralWord(WordToken("ref4"))))
                ),
                LiteralObjectItem(
                  WordToken("e"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1.0))),
                        LiteralObjectItem(
                          WordToken("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref5")))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                LiteralObjectItem(
                  WordToken("f"),
                  LiteralProto(
                    WordToken("g"),
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1.0))),
                        LiteralObjectItem(
                          WordToken("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref6")))
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
          ),
          ObjectType,
          hasReference = true,
          isTopLevel = true
        ),
        IndexRow(
          WordToken("a"),
          List(WordToken("foo")),
          LiteralBoolGroup(
            LiteralBoolOperatorAnd(),
            LiteralBoolFactor(BoolToken(false)),
            LiteralBoolWord(WordToken("ref1"))
          ),
          BoolType,
          hasReference = true
        ),
        IndexRow(
          WordToken("b"),
          List(WordToken("foo")),
          LiteralStringGroup(
            LiteralStringOperatorConcat(),
            LiteralStringFactor(StringToken("abc")),
            LiteralStringWord(WordToken("ref2"))
          ),
          StringType,
          hasReference = true
        ),
        IndexRow(
          WordToken("c"),
          List(WordToken("foo")),
          LiteralNumberGroup(
            LiteralNumberOperatorAdd(),
            LiteralNumberFactor(NumberToken(1)),
            LiteralNumberWord(WordToken("ref3"))
          ),
          NumberType,
          hasReference = true
        ),
        IndexRow(
          WordToken("d"),
          List(WordToken("foo")),
          LiteralArray(List(LiteralWord(WordToken("ref4")))),
          ArrayType,
          hasReference = true
        ),
        IndexRow(
          WordToken("e"),
          List(WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  WordToken("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref5")))
                      )
                    )
                  )
                )
              )
            )
          ),
          ObjectType,
          hasReference = true
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("e"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          NumberType,
          hasReference = false
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("e"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref5"))))
            )
          ),
          ObjectType,
          hasReference = true
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("e"), WordToken("foo")),
          LiteralWord(WordToken("ref5")),
          WordType,
          hasReference = true
        ),
        IndexRow(
          WordToken("f"),
          List(WordToken("foo")),
          LiteralProto(
            WordToken("g"),
            LiteralObjectItems(
              List(
                LiteralObjectItem(WordToken("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  WordToken("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref6")))
                      )
                    )
                  )
                )
              )
            )
          ),
          ProtoType,
          hasReference = true
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("f"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          NumberType,
          hasReference = false
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("f"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralWord(WordToken("ref6"))))
            )
          ),
          ObjectType,
          hasReference = true
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("f"), WordToken("foo")),
          LiteralWord(WordToken("ref6")),
          WordType,
          hasReference = true
        )
      )
    }
  }

  describe("Index lookup") {
    it("should be sensitive to the type (unrealistic conf because of duplicated item names)") {
      val index = indexStmts("""conf foo : Foo {
          |     a = true
          |     a = 1.0
          |     a = "abc"
          |     a = [1.0]
          |     a = { x = 1.0 }
          |     a = x { y = 1.0 }
          |}""".stripMargin)

      val name    = WordToken("a")
      val pos     = name.pos
      val parents = List(WordToken("foo"))

      val bool   = indexLookup[LiteralBool](name, BoolType, pos, parents, index)
      val number = indexLookup[LiteralNumber](name, NumberType, pos, parents, index)
      val string = indexLookup[LiteralString](name, StringType, pos, parents, index)
      val array  = indexLookup[LiteralArray](name, ArrayType, pos, parents, index)
      val obj    = indexLookup[LiteralObject](name, ObjectType, pos, parents, index)
      val proto  = indexLookup[LiteralProto](name, ProtoType, pos, parents, index)

      bool shouldEqual LiteralBoolFactor(BoolToken(true))
      number shouldEqual LiteralNumberFactor(NumberToken(1.0))
      string shouldEqual LiteralStringFactor(StringToken("abc"))
      array shouldEqual LiteralArray(List(LiteralNumberFactor(NumberToken(1.0))))
      obj shouldEqual LiteralObject(
        LiteralObjectItems(
          List(LiteralObjectItem(WordToken("x"), LiteralNumberFactor(NumberToken(1.0))))
        )
      )
      proto shouldEqual LiteralProto(
        WordToken("x"),
        LiteralObjectItems(
          List(LiteralObjectItem(WordToken("y"), LiteralNumberFactor(NumberToken(1.0))))
        )
      )

    }
  }

  def indexStmts(input: String): List[IndexRow] = {
    (for {
      tokens <- ConfeeLexer(input)
      parsed <- ConfeeParser(tokens)
    } yield parsed) match {
      case Right(Grammar(stmts: List[Stmt])) => ConfeeIndexer.indexStmts(stmts)
      case error                             => fail(error.toString)
    }
  }

}
