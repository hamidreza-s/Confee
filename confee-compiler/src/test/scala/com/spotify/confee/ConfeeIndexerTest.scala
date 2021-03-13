package com.spotify.confee

import com.spotify.confee.ConfeeIndexer._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeIndexerTest extends AnyFunSpec with Matchers {

  describe("Indexer on type type") {
    it("should index types of bool, number, string and custom defined object") {
      indexTypeStmts("""type Foo {
                       |     a: Bool
                       |     b: Number
                       |     c: String
                       |     d: Bar
                       |}
                       |
                       |type Bar {
                       |     a: [Bool]
                       |     b: [Number]
                       |     c: [String]
                       |     d: [Foo]
                       |}""".stripMargin) shouldEqual List(
        TypeIndex("Foo", "a", "Bool", BoolDefinedType, isList = false),
        TypeIndex("Foo", "b", "Number", NumberDefinedType, isList = false),
        TypeIndex("Foo", "c", "String", StringDefinedType, isList = false),
        TypeIndex("Foo", "d", "Bar", ObjectDefinedType, isList = false),
        TypeIndex("Bar", "a", "Bool", BoolDefinedType, isList = true),
        TypeIndex("Bar", "b", "Number", NumberDefinedType, isList = true),
        TypeIndex("Bar", "c", "String", StringDefinedType, isList = true),
        TypeIndex("Bar", "d", "Foo", ObjectDefinedType, isList = true)
      )
    }
  }

  describe("Indexer on conf statements") {
    it("should index config items and its object or proto children WITHOUT reference") {
      indexConfStmts("""conf foo : Foo {
          |     a = true
          |     b = "abc"
          |     c = 1
          |     d = [1]
          |     e = {bar = 1 bat = {ban = 2}}
          |     f = g {bar = 1 bat = {ban = 2}}
          |}""".stripMargin) shouldEqual List(
        ConfIndex(
          "foo",
          List(),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("a"), LiteralBoolFactor(BoolToken(true))),
                LiteralObjectItem(
                  LiteralObjectItemKey("b"),
                  LiteralStringFactor(StringToken("abc"))
                ),
                LiteralObjectItem(LiteralObjectItemKey("c"), LiteralNumberFactor(NumberToken(1.0))),
                LiteralObjectItem(
                  LiteralObjectItemKey("d"),
                  LiteralArray(List(LiteralNumberFactor(NumberToken(1.0))))
                ),
                LiteralObjectItem(
                  LiteralObjectItemKey("e"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("bar"),
                          LiteralNumberFactor(NumberToken(1.0))
                        ),
                        LiteralObjectItem(
                          LiteralObjectItemKey("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(
                                  LiteralObjectItemKey("ban"),
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
                  LiteralObjectItemKey("f"),
                  LiteralProto(
                    LiteralProtoKey("g"),
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("bar"),
                          LiteralNumberFactor(NumberToken(1.0))
                        ),
                        LiteralObjectItem(
                          LiteralObjectItemKey("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(
                                  LiteralObjectItemKey("ban"),
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
          ObjectInferredType,
          hasReference = false,
          isTopLevel = true
        ),
        ConfIndex(
          "a",
          List("foo"),
          LiteralBoolFactor(BoolToken(true)),
          BoolInferredType,
          hasReference = false
        ),
        ConfIndex(
          "b",
          List("foo"),
          LiteralStringFactor(StringToken("abc")),
          StringInferredType,
          hasReference = false
        ),
        ConfIndex(
          "c",
          List("foo"),
          LiteralNumberFactor(NumberToken(1)),
          NumberInferredType,
          hasReference = false
        ),
        ConfIndex(
          "d",
          List("foo"),
          LiteralArray(List(LiteralNumberFactor(NumberToken(1.0)))),
          ArrayInferredType,
          hasReference = false
        ),
        ConfIndex(
          "e",
          List("foo"),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  LiteralObjectItemKey("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("ban"),
                          LiteralNumberFactor(NumberToken(2))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          ObjectInferredType,
          hasReference = false
        ),
        ConfIndex(
          "bar",
          List("e", "foo"),
          LiteralNumberFactor(NumberToken(1)),
          NumberInferredType,
          hasReference = false
        ),
        ConfIndex(
          "bat",
          List("e", "foo"),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralNumberFactor(NumberToken(2)))
              )
            )
          ),
          ObjectInferredType,
          hasReference = false
        ),
        ConfIndex(
          "ban",
          List("bat", "e", "foo"),
          LiteralNumberFactor(NumberToken(2)),
          NumberInferredType,
          hasReference = false
        ),
        ConfIndex(
          "f",
          List("foo"),
          LiteralProto(
            LiteralProtoKey("g"),
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  LiteralObjectItemKey("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("ban"),
                          LiteralNumberFactor(NumberToken(2))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          ProtoInferredType,
          hasReference = false
        ),
        ConfIndex(
          "bar",
          List("f", "foo"),
          LiteralNumberFactor(NumberToken(1)),
          NumberInferredType,
          hasReference = false
        ),
        ConfIndex(
          "bat",
          List("f", "foo"),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralNumberFactor(NumberToken(2)))
              )
            )
          ),
          ObjectInferredType,
          hasReference = false
        ),
        ConfIndex(
          "ban",
          List("bat", "f", "foo"),
          LiteralNumberFactor(NumberToken(2)),
          NumberInferredType,
          hasReference = false
        )
      )
    }

    it("should index config items and its object or proto children WITH reference") {
      indexConfStmts("""conf foo : Foo {
          |     a = false and ref1
          |     b = "abc" + ref2
          |     c = 1 + ref3
          |     d = [ref4]
          |     e = {bar = 1 bat = {ban = ref5}}
          |     f = g {bar = 1 bat = {ban = ref6}}
          |}""".stripMargin) shouldEqual List(
        ConfIndex(
          "foo",
          List(),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(
                  LiteralObjectItemKey("a"),
                  LiteralBoolGroup(
                    LiteralBoolOperatorAnd(),
                    LiteralBoolFactor(BoolToken(false)),
                    LiteralBoolWord(WordToken("ref1"))
                  )
                ),
                LiteralObjectItem(
                  LiteralObjectItemKey("b"),
                  LiteralStringGroup(
                    LiteralStringOperatorConcat(),
                    LiteralStringFactor(StringToken("abc")),
                    LiteralStringWord(WordToken("ref2"))
                  )
                ),
                LiteralObjectItem(
                  LiteralObjectItemKey("c"),
                  LiteralNumberGroup(
                    LiteralNumberOperatorAdd(),
                    LiteralNumberFactor(NumberToken(1.0)),
                    LiteralNumberWord(WordToken("ref3"))
                  )
                ),
                LiteralObjectItem(
                  LiteralObjectItemKey("d"),
                  LiteralArray(List(LiteralWord(WordToken("ref4"))))
                ),
                LiteralObjectItem(
                  LiteralObjectItemKey("e"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("bar"),
                          LiteralNumberFactor(NumberToken(1.0))
                        ),
                        LiteralObjectItem(
                          LiteralObjectItemKey("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(
                                  LiteralObjectItemKey("ban"),
                                  LiteralWord(WordToken("ref5"))
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
                  LiteralObjectItemKey("f"),
                  LiteralProto(
                    LiteralProtoKey("g"),
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("bar"),
                          LiteralNumberFactor(NumberToken(1.0))
                        ),
                        LiteralObjectItem(
                          LiteralObjectItemKey("bat"),
                          LiteralObject(
                            LiteralObjectItems(
                              List(
                                LiteralObjectItem(
                                  LiteralObjectItemKey("ban"),
                                  LiteralWord(WordToken("ref6"))
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
          ObjectInferredType,
          hasReference = true,
          isTopLevel = true
        ),
        ConfIndex(
          "a",
          List("foo"),
          LiteralBoolGroup(
            LiteralBoolOperatorAnd(),
            LiteralBoolFactor(BoolToken(false)),
            LiteralBoolWord(WordToken("ref1"))
          ),
          BoolInferredType,
          hasReference = true
        ),
        ConfIndex(
          "b",
          List("foo"),
          LiteralStringGroup(
            LiteralStringOperatorConcat(),
            LiteralStringFactor(StringToken("abc")),
            LiteralStringWord(WordToken("ref2"))
          ),
          StringInferredType,
          hasReference = true
        ),
        ConfIndex(
          "c",
          List("foo"),
          LiteralNumberGroup(
            LiteralNumberOperatorAdd(),
            LiteralNumberFactor(NumberToken(1)),
            LiteralNumberWord(WordToken("ref3"))
          ),
          NumberInferredType,
          hasReference = true
        ),
        ConfIndex(
          "d",
          List("foo"),
          LiteralArray(List(LiteralWord(WordToken("ref4")))),
          ArrayInferredType,
          hasReference = true
        ),
        ConfIndex(
          "e",
          List("foo"),
          LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  LiteralObjectItemKey("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("ban"),
                          LiteralWord(WordToken("ref5"))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          ObjectInferredType,
          hasReference = true
        ),
        ConfIndex(
          "bar",
          List("e", "foo"),
          LiteralNumberFactor(NumberToken(1)),
          NumberInferredType,
          hasReference = false
        ),
        ConfIndex(
          "bat",
          List("e", "foo"),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralWord(WordToken("ref5"))))
            )
          ),
          ObjectInferredType,
          hasReference = true
        ),
        ConfIndex(
          "ban",
          List("bat", "e", "foo"),
          LiteralWord(WordToken("ref5")),
          WordInferredType,
          hasReference = true
        ),
        ConfIndex(
          "f",
          List("foo"),
          LiteralProto(
            LiteralProtoKey("g"),
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("bar"), LiteralNumberFactor(NumberToken(1))),
                LiteralObjectItem(
                  LiteralObjectItemKey("bat"),
                  LiteralObject(
                    LiteralObjectItems(
                      List(
                        LiteralObjectItem(
                          LiteralObjectItemKey("ban"),
                          LiteralWord(WordToken("ref6"))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          ProtoInferredType,
          hasReference = true
        ),
        ConfIndex(
          "bar",
          List("f", "foo"),
          LiteralNumberFactor(NumberToken(1)),
          NumberInferredType,
          hasReference = false
        ),
        ConfIndex(
          "bat",
          List("f", "foo"),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralWord(WordToken("ref6"))))
            )
          ),
          ObjectInferredType,
          hasReference = true
        ),
        ConfIndex(
          "ban",
          List("bat", "f", "foo"),
          LiteralWord(WordToken("ref6")),
          WordInferredType,
          hasReference = true
        )
      )
    }
  }

  describe("Index lookup") {
    it("should be sensitive to the type (unrealistic conf because of duplicated item names)") {
      val index = indexConfStmts("""conf foo : Foo {
          |     a = true
          |     a = 1.0
          |     a = "abc"
          |     a = [1.0]
          |     a = { x = 1.0 }
          |     a = x { y = 1.0 }
          |}""".stripMargin)

      val name    = WordToken("a")
      val key     = name.word
      val pos     = name.pos
      val parents = List("foo")

      val bool   = indexLookup[LiteralBool](key, BoolInferredType, pos, parents, index)
      val number = indexLookup[LiteralNumber](key, NumberInferredType, pos, parents, index)
      val string = indexLookup[LiteralString](key, StringInferredType, pos, parents, index)
      val array  = indexLookup[LiteralArray](key, ArrayInferredType, pos, parents, index)
      val obj    = indexLookup[LiteralObject](key, ObjectInferredType, pos, parents, index)
      val proto  = indexLookup[LiteralProto](key, ProtoInferredType, pos, parents, index)

      bool shouldEqual LiteralBoolFactor(BoolToken(true))
      number shouldEqual LiteralNumberFactor(NumberToken(1.0))
      string shouldEqual LiteralStringFactor(StringToken("abc"))
      array shouldEqual LiteralArray(List(LiteralNumberFactor(NumberToken(1.0))))
      obj shouldEqual LiteralObject(
        LiteralObjectItems(
          List(LiteralObjectItem(LiteralObjectItemKey("x"), LiteralNumberFactor(NumberToken(1.0))))
        )
      )
      proto shouldEqual LiteralProto(
        LiteralProtoKey("x"),
        LiteralObjectItems(
          List(LiteralObjectItem(LiteralObjectItemKey("y"), LiteralNumberFactor(NumberToken(1.0))))
        )
      )

    }
  }

  def indexTypeStmts(input: String): List[TypeIndex] = {
    (for {
      tokens <- ConfeeLexer(input)
      parsed <- ConfeeParser(tokens)
    } yield parsed) match {
      case Right(Grammar(stmts: List[Stmt])) => ConfeeIndexer.indexTypeStmts(stmts)
      case error                             => fail(error.toString)
    }
  }

  def indexConfStmts(input: String): List[ConfIndex] = {
    (for {
      tokens <- ConfeeLexer(input)
      parsed <- ConfeeParser(tokens)
    } yield parsed) match {
      case Right(Grammar(stmts: List[Stmt])) => ConfeeIndexer.indexConfStmts(stmts)
      case error                             => fail(error.toString)
    }
  }

}
