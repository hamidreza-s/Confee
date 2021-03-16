package com.spotify.confee

import com.spotify.confee.ConfeeIndexer._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.util.parsing.input.NoPosition

class ConfeeIndexerTest extends AnyFunSpec with Matchers {

  describe("Indexer on type statement") {
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
        TypeIndex(
          "Foo",
          Map(
            "a" -> BoolDefinedType(isList = false),
            "b" -> NumberDefinedType(isList = false),
            "c" -> StringDefinedType(isList = false),
            "d" -> ObjectDefinedType("Bar", isList = false)
          )
        ),
        TypeIndex(
          "Bar",
          Map(
            "a" -> BoolDefinedType(isList = true),
            "b" -> NumberDefinedType(isList = true),
            "c" -> StringDefinedType(isList = true),
            "d" -> ObjectDefinedType("Foo", isList = true)
          )
        )
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
          Some(ObjectDefinedType("Foo", isList = false)),
          List(),
          isTopLevel = true
        ),
        ConfIndex(
          "a",
          expr = LiteralBoolFactor(BoolToken(true)),
          inferredType = BoolInferredType,
          parents = List("foo")
        ),
        ConfIndex(
          "b",
          expr = LiteralStringFactor(StringToken("abc")),
          inferredType = StringInferredType,
          parents = List("foo")
        ),
        ConfIndex(
          "c",
          expr = LiteralNumberFactor(NumberToken(1)),
          inferredType = NumberInferredType,
          parents = List("foo")
        ),
        ConfIndex(
          "d",
          expr = LiteralArray(List(LiteralNumberFactor(NumberToken(1.0)))),
          inferredType = ArrayInferredType,
          parents = List("foo")
        ),
        ConfIndex(
          "e",
          expr = LiteralObject(
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
          inferredType = ObjectInferredType,
          parents = List("foo")
        ),
        ConfIndex(
          "bar",
          expr = LiteralNumberFactor(NumberToken(1)),
          inferredType = NumberInferredType,
          parents = List("e", "foo")
        ),
        ConfIndex(
          "bat",
          expr = LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralNumberFactor(NumberToken(2)))
              )
            )
          ),
          inferredType = ObjectInferredType,
          parents = List("e", "foo")
        ),
        ConfIndex(
          "ban",
          expr = LiteralNumberFactor(NumberToken(2)),
          inferredType = NumberInferredType,
          parents = List("bat", "e", "foo")
        ),
        ConfIndex(
          "f",
          expr = LiteralProto(
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
          inferredType = ProtoInferredType,
          parents = List("foo")
        ),
        ConfIndex(
          "bar",
          expr = LiteralNumberFactor(NumberToken(1)),
          inferredType = NumberInferredType,
          parents = List("f", "foo")
        ),
        ConfIndex(
          "bat",
          expr = LiteralObject(
            LiteralObjectItems(
              List(
                LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralNumberFactor(NumberToken(2)))
              )
            )
          ),
          inferredType = ObjectInferredType,
          parents = List("f", "foo")
        ),
        ConfIndex(
          "ban",
          expr = LiteralNumberFactor(NumberToken(2)),
          inferredType = NumberInferredType,
          parents = List("bat", "f", "foo")
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
          Some(ObjectDefinedType("Foo", isList = false)),
          List(),
          isTopLevel = true,
          hasReference = true
        ),
        ConfIndex(
          "a",
          expr = LiteralBoolGroup(
            LiteralBoolOperatorAnd(),
            LiteralBoolFactor(BoolToken(false)),
            LiteralBoolWord(WordToken("ref1"))
          ),
          inferredType = BoolInferredType,
          parents = List("foo"),
          hasReference = true
        ),
        ConfIndex(
          "b",
          expr = LiteralStringGroup(
            LiteralStringOperatorConcat(),
            LiteralStringFactor(StringToken("abc")),
            LiteralStringWord(WordToken("ref2"))
          ),
          inferredType = StringInferredType,
          parents = List("foo"),
          hasReference = true
        ),
        ConfIndex(
          "c",
          expr = LiteralNumberGroup(
            LiteralNumberOperatorAdd(),
            LiteralNumberFactor(NumberToken(1)),
            LiteralNumberWord(WordToken("ref3"))
          ),
          inferredType = NumberInferredType,
          parents = List("foo"),
          hasReference = true
        ),
        ConfIndex(
          "d",
          expr = LiteralArray(List(LiteralWord(WordToken("ref4")))),
          inferredType = ArrayInferredType,
          parents = List("foo"),
          hasReference = true
        ),
        ConfIndex(
          "e",
          expr = LiteralObject(
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
          inferredType = ObjectInferredType,
          parents = List("foo"),
          hasReference = true
        ),
        ConfIndex(
          "bar",
          expr = LiteralNumberFactor(NumberToken(1)),
          inferredType = NumberInferredType,
          parents = List("e", "foo")
        ),
        ConfIndex(
          "bat",
          expr = LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralWord(WordToken("ref5"))))
            )
          ),
          inferredType = ObjectInferredType,
          parents = List("e", "foo"),
          hasReference = true
        ),
        ConfIndex(
          "ban",
          expr = LiteralWord(WordToken("ref5")),
          inferredType = WordInferredType,
          parents = List("bat", "e", "foo"),
          hasReference = true
        ),
        ConfIndex(
          "f",
          expr = LiteralProto(
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
          inferredType = ProtoInferredType,
          parents = List("foo"),
          hasReference = true
        ),
        ConfIndex(
          "bar",
          expr = LiteralNumberFactor(NumberToken(1)),
          inferredType = NumberInferredType,
          parents = List("f", "foo")
        ),
        ConfIndex(
          "bat",
          expr = LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(LiteralObjectItemKey("ban"), LiteralWord(WordToken("ref6"))))
            )
          ),
          inferredType = ObjectInferredType,
          parents = List("f", "foo"),
          hasReference = true
        ),
        ConfIndex(
          "ban",
          expr = LiteralWord(WordToken("ref6")),
          inferredType = WordInferredType,
          parents = List("bat", "f", "foo"),
          hasReference = true
        )
      )
    }
  }

  describe("Indexer on conf statements paired with type statements") {
    it("should match conf with correct type with ONE level nesting") {
      indexStmts("""
          |type FooType {
          |     fooK1 : Bool
          |     fooK2 : Number
          |     fooK3 : String
          |     fooK4 : [Bool]
          |     fooK5 : [Number]
          |     fooK6 : [String]
          |}
          |
          |type BarType {
          |     barK1 : FooType
          |     barK2 : [FooType]
          |}
          |
          |conf foo : FooType {
          |     fooK1 = true
          |     fooK2 = 1.0
          |     fooK3 = "abc"
          |     fooK4 = [true, false]
          |     fooK5 = [1.0, 2.0, 3.0]
          |     fooK6 = ["a", "b", "c"]
          |}
          |
          |conf bar : BarType {
          |     barK1 = foo
          |     barK2 = [
          |          foo { f2 = 2.0 },
          |          foo { f2 = 3.0 }
          |     ]
          |}
          |""".stripMargin) shouldEqual List(
        Index(
          ConfIndex(
            "foo",
            LiteralObject(
              LiteralObjectItems(
                List(
                  LiteralObjectItem(
                    LiteralObjectItemKey("fooK1"),
                    LiteralBoolFactor(BoolToken(true))
                  ),
                  LiteralObjectItem(
                    LiteralObjectItemKey("fooK2"),
                    LiteralNumberFactor(NumberToken(1.0))
                  ),
                  LiteralObjectItem(
                    LiteralObjectItemKey("fooK3"),
                    LiteralStringFactor(StringToken("abc"))
                  ),
                  LiteralObjectItem(
                    LiteralObjectItemKey("fooK4"),
                    LiteralArray(
                      List(LiteralBoolFactor(BoolToken(true)), LiteralBoolFactor(BoolToken(false)))
                    )
                  ),
                  LiteralObjectItem(
                    LiteralObjectItemKey("fooK5"),
                    LiteralArray(
                      List(
                        LiteralNumberFactor(NumberToken(1.0)),
                        LiteralNumberFactor(NumberToken(2.0)),
                        LiteralNumberFactor(NumberToken(3.0))
                      )
                    )
                  ),
                  LiteralObjectItem(
                    LiteralObjectItemKey("fooK6"),
                    LiteralArray(
                      List(
                        LiteralStringFactor(StringToken("a")),
                        LiteralStringFactor(StringToken("b")),
                        LiteralStringFactor(StringToken("c"))
                      )
                    )
                  )
                )
              )
            ),
            ObjectInferredType,
            Some(ObjectDefinedType("FooType", isList = false)),
            List(),
            isTopLevel = true
          ),
          Some(
            TypeIndex(
              "FooType",
              Map(
                "fooK3" -> StringDefinedType(false),
                "fooK1" -> BoolDefinedType(false),
                "fooK4" -> BoolDefinedType(true),
                "fooK5" -> NumberDefinedType(true),
                "fooK6" -> StringDefinedType(true),
                "fooK2" -> NumberDefinedType(false)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "fooK1",
            LiteralBoolFactor(BoolToken(true)),
            BoolInferredType,
            None,
            List("foo")
          ),
          Some(
            TypeIndex(
              "FooType",
              Map(
                "fooK3" -> StringDefinedType(false),
                "fooK1" -> BoolDefinedType(false),
                "fooK4" -> BoolDefinedType(true),
                "fooK5" -> NumberDefinedType(true),
                "fooK6" -> StringDefinedType(true),
                "fooK2" -> NumberDefinedType(false)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "fooK2",
            LiteralNumberFactor(NumberToken(1.0)),
            NumberInferredType,
            None,
            List("foo")
          ),
          Some(
            TypeIndex(
              "FooType",
              Map(
                "fooK3" -> StringDefinedType(false),
                "fooK1" -> BoolDefinedType(false),
                "fooK4" -> BoolDefinedType(true),
                "fooK5" -> NumberDefinedType(true),
                "fooK6" -> StringDefinedType(true),
                "fooK2" -> NumberDefinedType(false)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "fooK3",
            LiteralStringFactor(StringToken("abc")),
            StringInferredType,
            None,
            List("foo")
          ),
          Some(
            TypeIndex(
              "FooType",
              Map(
                "fooK3" -> StringDefinedType(false),
                "fooK1" -> BoolDefinedType(false),
                "fooK4" -> BoolDefinedType(true),
                "fooK5" -> NumberDefinedType(true),
                "fooK6" -> StringDefinedType(true),
                "fooK2" -> NumberDefinedType(false)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "fooK4",
            LiteralArray(
              List(LiteralBoolFactor(BoolToken(true)), LiteralBoolFactor(BoolToken(false)))
            ),
            ArrayInferredType,
            None,
            List("foo")
          ),
          Some(
            TypeIndex(
              "FooType",
              Map(
                "fooK3" -> StringDefinedType(false),
                "fooK1" -> BoolDefinedType(false),
                "fooK4" -> BoolDefinedType(true),
                "fooK5" -> NumberDefinedType(true),
                "fooK6" -> StringDefinedType(true),
                "fooK2" -> NumberDefinedType(false)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "fooK5",
            LiteralArray(
              List(
                LiteralNumberFactor(NumberToken(1.0)),
                LiteralNumberFactor(NumberToken(2.0)),
                LiteralNumberFactor(NumberToken(3.0))
              )
            ),
            ArrayInferredType,
            None,
            List("foo")
          ),
          Some(
            TypeIndex(
              "FooType",
              Map(
                "fooK3" -> StringDefinedType(false),
                "fooK1" -> BoolDefinedType(false),
                "fooK4" -> BoolDefinedType(true),
                "fooK5" -> NumberDefinedType(true),
                "fooK6" -> StringDefinedType(true),
                "fooK2" -> NumberDefinedType(false)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "fooK6",
            LiteralArray(
              List(
                LiteralStringFactor(StringToken("a")),
                LiteralStringFactor(StringToken("b")),
                LiteralStringFactor(StringToken("c"))
              )
            ),
            ArrayInferredType,
            None,
            List("foo")
          ),
          Some(
            TypeIndex(
              "FooType",
              Map(
                "fooK3" -> StringDefinedType(false),
                "fooK1" -> BoolDefinedType(false),
                "fooK4" -> BoolDefinedType(true),
                "fooK5" -> NumberDefinedType(true),
                "fooK6" -> StringDefinedType(true),
                "fooK2" -> NumberDefinedType(false)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "bar",
            LiteralObject(
              LiteralObjectItems(
                List(
                  LiteralObjectItem(LiteralObjectItemKey("barK1"), LiteralWord(WordToken("foo"))),
                  LiteralObjectItem(
                    LiteralObjectItemKey("barK2"),
                    LiteralArray(
                      List(
                        LiteralProto(
                          LiteralProtoKey("foo"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("f2"),
                                LiteralNumberFactor(NumberToken(2.0))
                              )
                            )
                          )
                        ),
                        LiteralProto(
                          LiteralProtoKey("foo"),
                          LiteralObjectItems(
                            List(
                              LiteralObjectItem(
                                LiteralObjectItemKey("f2"),
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
            ),
            ObjectInferredType,
            Some(ObjectDefinedType("BarType", isList = false)),
            List(),
            isTopLevel = true,
            hasReference = true
          ),
          Some(
            TypeIndex(
              "BarType",
              Map(
                "barK1" -> ObjectDefinedType("FooType", isList = false),
                "barK2" -> ObjectDefinedType("FooType", isList = true)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "barK1",
            LiteralWord(WordToken("foo")),
            WordInferredType,
            None,
            List("bar"),
            hasReference = true
          ),
          Some(
            TypeIndex(
              "BarType",
              Map(
                "barK1" -> ObjectDefinedType("FooType", isList = false),
                "barK2" -> ObjectDefinedType("FooType", isList = true)
              )
            )
          )
        ),
        Index(
          ConfIndex(
            "barK2",
            LiteralArray(
              List(
                LiteralProto(
                  LiteralProtoKey("foo"),
                  LiteralObjectItems(
                    List(
                      LiteralObjectItem(
                        LiteralObjectItemKey("f2"),
                        LiteralNumberFactor(NumberToken(2.0))
                      )
                    )
                  )
                ),
                LiteralProto(
                  LiteralProtoKey("foo"),
                  LiteralObjectItems(
                    List(
                      LiteralObjectItem(
                        LiteralObjectItemKey("f2"),
                        LiteralNumberFactor(NumberToken(3.0))
                      )
                    )
                  )
                )
              )
            ),
            ArrayInferredType,
            None,
            List("bar")
          ),
          Some(
            TypeIndex(
              "BarType",
              Map(
                "barK1" -> ObjectDefinedType("FooType", isList = false),
                "barK2" -> ObjectDefinedType("FooType", isList = true)
              )
            )
          )
        )
      )
    }

    it("should match conf with correct type with FOUR levels nesting") {
      indexStmts("""
          |type L4 {
          |     l3 : L3
          |}
          |
          |type L3 {
          |     l2 : L2
          |}
          |
          |type L2 {
          |     l1 : L1
          |}
          |
          |type L1 {
          |     l0 : String
          |}
          |
          |conf l4 : L4 {
          |     l3 = {
          |          l2 = {
          |               l1 = {
          |                    l0 = "you made it"
          |               }
          |          }
          |     }
          |}
          |""".stripMargin) shouldEqual List(
        Index(
          ConfIndex(
            "l4",
            LiteralObject(
              LiteralObjectItems(
                List(
                  LiteralObjectItem(
                    LiteralObjectItemKey("l3"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("l2"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("l1"),
                                    LiteralObject(
                                      LiteralObjectItems(
                                        List(
                                          LiteralObjectItem(
                                            LiteralObjectItemKey("l0"),
                                            LiteralStringFactor(StringToken("you made it"))
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
            ),
            ObjectInferredType,
            Some(ObjectDefinedType("L4", isList = false)),
            List(),
            isTopLevel = true
          ),
          Some(TypeIndex("L4", Map("l3" -> ObjectDefinedType("L3", isList = false))))
        ),
        Index(
          ConfIndex(
            "l3",
            LiteralObject(
              LiteralObjectItems(
                List(
                  LiteralObjectItem(
                    LiteralObjectItemKey("l2"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("l1"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("l0"),
                                    LiteralStringFactor(StringToken("you made it"))
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
            None,
            List("l4")
          ),
          Some(TypeIndex("L4", Map("l3" -> ObjectDefinedType("L3", isList = false))))
        ),
        Index(
          ConfIndex(
            "l2",
            LiteralObject(
              LiteralObjectItems(
                List(
                  LiteralObjectItem(
                    LiteralObjectItemKey("l1"),
                    LiteralObject(
                      LiteralObjectItems(
                        List(
                          LiteralObjectItem(
                            LiteralObjectItemKey("l0"),
                            LiteralStringFactor(StringToken("you made it"))
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            ObjectInferredType,
            None,
            List("l3", "l4")
          ),
          Some(TypeIndex("L3", Map("l2" -> ObjectDefinedType("L2", isList = false))))
        ),
        Index(
          ConfIndex(
            "l1",
            LiteralObject(
              LiteralObjectItems(
                List(
                  LiteralObjectItem(
                    LiteralObjectItemKey("l0"),
                    LiteralStringFactor(StringToken("you made it"))
                  )
                )
              )
            ),
            ObjectInferredType,
            None,
            List("l2", "l3", "l4")
          ),
          Some(TypeIndex("L2", Map("l1" -> ObjectDefinedType("L1", isList = false))))
        ),
        Index(
          ConfIndex(
            "l0",
            LiteralStringFactor(StringToken("you made it")),
            StringInferredType,
            None,
            List("l1", "l2", "l3", "l4")
          ),
          Some(TypeIndex("L1", Map("l0" -> StringDefinedType(false))))
        )
      )
    }
  }

  describe("Index lookup for type") {
    it("should lookup types of conf and conf items with ONE level nesting") {
      val input =
        """
          |type FooType {
          |     fooK1 : Bool
          |     fooK2 : Number
          |     fooK3 : String
          |     fooK4 : [Bool]
          |     fooK5 : [Number]
          |     fooK6 : [String]
          |}
          |
          |type BarType {
          |     barK1 : FooType
          |     barK2 : [FooType]
          |}
          |
          |conf foo : FooType {
          |     fooK1 = true
          |     fooK2 = 1.0
          |     fooK3 = "abc"
          |     fooK4 = [true, false]
          |     fooK5 = [1.0, 2.0, 3.0]
          |     fooK6 = ["a", "b", "c"]
          |}
          |
          |conf bar : BarType {
          |     barK1 = foo
          |     barK2 = [
          |          foo { f2 = 2.0 },
          |          foo { f2 = 3.0 }
          |     ]
          |}
          |""".stripMargin

      val expectedFooType = TypeIndex(
        "FooType",
        Map(
          "fooK3" -> StringDefinedType(isList = false),
          "fooK1" -> BoolDefinedType(isList = false),
          "fooK4" -> BoolDefinedType(isList = true),
          "fooK5" -> NumberDefinedType(isList = true),
          "fooK6" -> StringDefinedType(isList = true),
          "fooK2" -> NumberDefinedType(isList = false)
        )
      )

      val expectedBarType = TypeIndex(
        "BarType",
        Map(
          "barK1" -> ObjectDefinedType("FooType", isList = false),
          "barK2" -> ObjectDefinedType("FooType", isList = true)
        )
      )

      val confStmtsIndex = indexConfStmts(input)
      val typeStmtsIndex = indexTypeStmts(input)
      val index          = indexStmts(input)

      /* ----- foo test ----- */

      val fooConf = topLevelConfIndexLookup("foo", confStmtsIndex)
      val fooType = typeIndexLookup(fooConf, typeStmtsIndex, confStmtsIndex)
      fooType shouldEqual expectedFooType

      /* ----- fooK1-6 test ----- */

      val fooK1To6 = List(
        (1, Some(BoolDefinedType(isList = false))),
        (2, Some(NumberDefinedType(isList = false))),
        (3, Some(StringDefinedType(isList = false))),
        (4, Some(BoolDefinedType(isList = true))),
        (5, Some(NumberDefinedType(isList = true))),
        (6, Some(StringDefinedType(isList = true)))
      )
      for ((i, expectedFooK) <- fooK1To6) {
        val fooKConf = confIndexLookup(
          s"fooK$i",
          NoPosition,
          List("foo"),
          confStmtsIndex
        )
        val fooKType =
          typeIndexLookup(fooKConf, typeStmtsIndex, confStmtsIndex).items.get(s"fooK$i")
        fooKType shouldEqual expectedFooK
      }

      /* ----- bar test ----- */

      val barConf = topLevelConfIndexLookup("bar", confStmtsIndex)
      val barType = typeIndexLookup(barConf, typeStmtsIndex, confStmtsIndex)
      barType shouldEqual expectedBarType

      /* ----- barK1 test ----- */

      val barK1Conf = confIndexLookup(
        "barK1",
        NoPosition,
        List("bar"),
        confStmtsIndex
      )
      val barK1Type = typeIndexLookup(barK1Conf, typeStmtsIndex, confStmtsIndex).items.get("barK1")
      barK1Type shouldEqual Some(ObjectDefinedType("FooType", isList = false))

      /* ----- barK2 test ----- */

      val barK2Conf = confIndexLookup(
        "barK2",
        NoPosition,
        List("bar"),
        confStmtsIndex
      )
      val barK2Type = typeIndexLookup(barK2Conf, typeStmtsIndex, confStmtsIndex).items.get("barK2")
      barK2Type shouldEqual Some(ObjectDefinedType("FooType", isList = true))
    }

    it("should lookup types of conf and conf items with up to FOUR levels nesting") {
      val input =
        """
          |type L4 {
          |     l3 : L3
          |}
          |
          |type L3 {
          |     l2 : L2
          |}
          |
          |type L2 {
          |     l1 : L1
          |}
          |
          |type L1 {
          |     l0 : String
          |}
          |
          |conf l4 : L4 {
          |     l3 = {
          |          l2 = {
          |               l1 = {
          |                    l0 = "you made it"
          |               }
          |          }
          |     }
          |}
          |""".stripMargin

      val confStmtsIndex = indexConfStmts(input)
      val typeStmtsIndex = indexTypeStmts(input)
      val index          = indexStmts(input)

      /* ----- level 4 test ----- */

      val l4Conf = topLevelConfIndexLookup("l4", confStmtsIndex)
      val l4Type = typeIndexLookup(l4Conf, typeStmtsIndex, confStmtsIndex)
      l4Type shouldEqual TypeIndex("L4", Map("l3" -> ObjectDefinedType("L3", isList = false)))

      /* ----- level 3 test ----- */

      val l3Conf = confIndexLookup(
        "l3",
        NoPosition,
        List("l4"),
        confStmtsIndex
      )
      val l3Type = typeIndexLookup(l3Conf, typeStmtsIndex, confStmtsIndex).items.get("l3")
      l3Type shouldEqual Some(ObjectDefinedType("L3", isList = false))

      /* ----- level 2 test ----- */

      val l2Conf = confIndexLookup(
        "l2",
        NoPosition,
        List("l3", "l4"),
        confStmtsIndex
      )
      val l2Type = typeIndexLookup(l2Conf, typeStmtsIndex, confStmtsIndex).items.get("l2")
      l2Type shouldEqual Some(ObjectDefinedType("L2", isList = false))

      /* ----- level 1 test ----- */

      val l1Conf = confIndexLookup(
        "l1",
        NoPosition,
        List("l2", "l3", "l4"),
        confStmtsIndex
      )
      val l1Type = typeIndexLookup(l1Conf, typeStmtsIndex, confStmtsIndex).items.get("l1")
      l1Type shouldEqual Some(ObjectDefinedType("L1", isList = false))

      /* ----- level 0 test ----- */

      val l0Conf = confIndexLookup(
        "l0",
        NoPosition,
        List("l1", "l2", "l3", "l4"),
        confStmtsIndex
      )
      val l0Type = typeIndexLookup(l0Conf, typeStmtsIndex, confStmtsIndex).items.get("l0")
      l0Type shouldEqual Some(StringDefinedType(isList = false))
    }
  }

  describe("Index lookup for conf") {
    it("should lookup conf with similar key names but different types in different confs") {
      val input =
        """
          |conf foo1 : FooOne {
          |     a = true
          |}
          |
          |conf foo2 : FooTwo {
          |     a = 1.0
          |}
          |""".stripMargin

      val index = indexConfStmts(input)

      confIndexLookup("foo1", NoPosition, List(), index) shouldEqual ConfIndex(
        "foo1",
        LiteralObject(
          LiteralObjectItems(
            List(LiteralObjectItem(LiteralObjectItemKey("a"), LiteralBoolFactor(BoolToken(true))))
          )
        ),
        ObjectInferredType,
        Some(ObjectDefinedType("FooOne", isList = false)),
        List(),
        isTopLevel = true
      )

      confIndexLookup("a", NoPosition, List("foo1"), index) shouldEqual ConfIndex(
        "a",
        LiteralBoolFactor(BoolToken(true)),
        BoolInferredType,
        None,
        List("foo1")
      )

      confIndexLookup("foo2", NoPosition, List(), index) shouldEqual ConfIndex(
        "foo2",
        LiteralObject(
          LiteralObjectItems(
            List(
              LiteralObjectItem(LiteralObjectItemKey("a"), LiteralNumberFactor(NumberToken(1.0)))
            )
          )
        ),
        ObjectInferredType,
        Some(ObjectDefinedType("FooTwo", isList = false)),
        List(),
        isTopLevel = true
      )

      confIndexLookup("a", NoPosition, List("foo2"), index) shouldEqual ConfIndex(
        "a",
        LiteralNumberFactor(NumberToken(1.0)),
        NumberInferredType,
        None,
        List("foo2")
      )
    }
  }

  def indexStmts(input: String): List[Index] = {
    (for {
      tokens <- ConfeeLexer(input)
      parsed <- ConfeeParser(tokens)
    } yield parsed) match {
      case Right(Grammar(stmts: List[Stmt])) => ConfeeIndexer.indexStmts(stmts)
      case error                             => fail(error.toString)
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
