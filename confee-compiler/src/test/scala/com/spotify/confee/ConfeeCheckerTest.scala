package com.spotify.confee

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeCheckerTest extends AnyFunSpec with Matchers {

  describe("Type Checker") {
    it("should return the given AST back without changes when there is no type error") {
      checkAST("""
          |type Foo {
          |     a : Bool
          |     b : Number
          |}
          |
          |conf foo : Foo {
          |     a = true
          |     b = 1.0
          |}
          |""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            TypeStmt(
              NameToken("Foo"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("a"), TypeDef(NameToken("Bool"), isList = false)),
                  TypeItem(TypeItemKey("b"), TypeDef(NameToken("Number"), isList = false))
                )
              )
            ),
            ConfStmt(
              WordToken("foo"),
              TypeDef(NameToken("Foo"), isList = false),
              ConfItems(
                List(
                  ConfItem(ConfItemKey("a"), LiteralBoolFactor(BoolToken(true))),
                  ConfItem(ConfItemKey("b"), LiteralNumberFactor(NumberToken(1.0)))
                )
              )
            )
          )
        )
      )

    }

    it("should return the given AST back without changes when there is no type error (multi-level)") {
      checkAST("""
          |type Foo {
          |     bar : Bar
          |}
          |type Bar {
          |     bat : Bat
          |}
          |type Bat {
          |     value : Number
          |}
          |conf foo : Foo {
          |     bar = {
          |          bat = {
          |               value = 1.0
          |          }
          |     }
          |}
          |""".stripMargin) shouldEqual Right(
        Grammar(
          List(
            TypeStmt(
              NameToken("Foo"),
              TypeItems(
                List(TypeItem(TypeItemKey("bar"), TypeDef(NameToken("Bar"), isList = false)))
              )
            ),
            TypeStmt(
              NameToken("Bar"),
              TypeItems(
                List(TypeItem(TypeItemKey("bat"), TypeDef(NameToken("Bat"), isList = false)))
              )
            ),
            TypeStmt(
              NameToken("Bat"),
              TypeItems(
                List(TypeItem(TypeItemKey("value"), TypeDef(NameToken("Number"), isList = false)))
              )
            ),
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
                            LiteralObjectItemKey("bat"),
                            LiteralObject(
                              LiteralObjectItems(
                                List(
                                  LiteralObjectItem(
                                    LiteralObjectItemKey("value"),
                                    LiteralNumberFactor(NumberToken(1.0))
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

    it("should return error when conf does not have defined type") {
      checkAST("""
          |conf foo : Foo {}
          |""".stripMargin) shouldEqual Left(
        ConfeeCheckerErrors(
          List(
            ConfeeCheckerError(Location(2, 1), "Type error: 'foo' conf does not have defined type")
          )
        )
      )
    }

    it("should return error when conf items has a wrong field name") {
      checkAST("""
                 |type Foo {
                 |     a : Number
                 |     b : Number
                 |}
                 |conf foo : Foo {
                 |     c = 1.0
                 |     d = 2.0
                 |}
                 |""".stripMargin) shouldEqual Left(
        ConfeeCheckerErrors(
          List(
            ConfeeCheckerError(
              Location(7, 10),
              "Type error: 'c' field name is not compatible with its type"
            ),
            ConfeeCheckerError(
              Location(8, 10),
              "Type error: 'd' field name is not compatible with its type"
            )
          )
        )
      )
    }

    it("should return error when conf items with primitive type has a wrong type (one-level)") {
      checkAST("""
          |type Foo {
          |     a : Bool
          |     b : Number
          |     c : String
          |}
          |type Bar {
          |     a : String
          |     b : Bool
          |     c : Number
          |}
          |conf foo : Bar {
          |     a = 1.0
          |     b = "wrong-type"
          |     c = true
          |}
          |""".stripMargin) shouldEqual Left(
        ConfeeCheckerErrors(
          List(
            ConfeeCheckerError(Location(13, 10), "Type error: 'a' value must have String type"),
            ConfeeCheckerError(Location(14, 10), "Type error: 'b' value must have Bool type"),
            ConfeeCheckerError(Location(15, 10), "Type error: 'c' value must have Number type")
          )
        )
      )
    }

    it("should return error when conf items with array type have wrong type (one-level)") {
      checkAST("""
                 |type Foo {
                 |     a : [Bool]
                 |     b : [Number]
                 |     c : [String]
                 |}
                 |type Bar {
                 |     a : [String]
                 |     b : [Bool]
                 |     c : [Number]
                 |}
                 |conf foo : Bar {
                 |     a = [1.0, "correct-type"]
                 |     b = ["wrong-type", true]
                 |     c = [true, 1.0]
                 |}
                 |""".stripMargin) shouldEqual Left(
        ConfeeCheckerErrors(
          List(
            ConfeeCheckerError(Location(13, 11), "Type error: 'a' value must have [String] type"),
            ConfeeCheckerError(Location(14, 11), "Type error: 'b' value must have [Bool] type"),
            ConfeeCheckerError(Location(15, 11), "Type error: 'c' value must have [Number] type")
          )
        )
      )
    }

    ignore("should return error when conf items with array of object type have wrong type") {
      checkAST("""
                 |type Pair {
                 |     k : Number
                 |     v : Number
                 |}
                 |type List {
                 |    items : [Pair]
                 |}
                 |conf list : List {
                 |     items = [
                 |          {
                 |               k = 1.0
                 |               v = 1.0
                 |          },
                 |          {
                 |               k = "wrong-type"
                 |               v = 2.0
                 |          }
                 |     ]
                 |}
                 |""".stripMargin) shouldEqual Left() // TODO: Fix it!
    }

    it("should return error when conf items with primitive type are NOT array type (one-level)") {
      checkAST("""
                 |type Foo {
                 |     a : [Bool]
                 |     b : [Number]
                 |     c : [String]
                 |}
                 |type Bar {
                 |     a : String
                 |     b : Bool
                 |     c : Number
                 |}
                 |conf foo : Bar {
                 |     a = ["should-not-be-array", "same-here"]
                 |     b = [false]
                 |     c = [1.0]
                 |}
                 |""".stripMargin) shouldEqual Left(
        ConfeeCheckerErrors(
          List(
            ConfeeCheckerError(Location(13, 11), "Type error: 'a' value must not be inside an array"),
            ConfeeCheckerError(Location(13, 34), "Type error: 'a' value must not be inside an array"),
            ConfeeCheckerError(Location(14, 11), "Type error: 'b' value must not be inside an array"),
            ConfeeCheckerError(Location(15, 11), "Type error: 'c' value must not be inside an array")
          )
        )
      )
    }

    it("should return error when conf items with object type have wrong type (one-level)") {
      checkAST("""
                 |type Data {
                 |     key : String
                 |     active : Bool
                 |}
                 |type Foo {
                 |     a : Data
                 |}
                 |conf foo : Foo {
                 |     a = {
                 |          key = "right-type"
                 |          active = "wrong-type"
                 |     }
                 |}
                 |""".stripMargin) shouldEqual Left(
        ConfeeCheckerErrors(
          List(
            ConfeeCheckerError(Location(12, 20), "Type error: 'active' value must have Bool type")
          )
        )
      )
    }

    it("should return error when conf items has a wrong type (multi-level)") {
      checkAST("""
                 |type Foo {
                 |     bar : Bar
                 |}
                 |type Bar {
                 |     bat : Bat
                 |}
                 |type Bat {
                 |     value : Number
                 |}
                 |type Bal {
                 |     value : String
                 |}
                 |
                 |conf foo : Foo {
                 |     bar = {
                 |          bat = {
                 |               value = "wrong-type"
                 |          }
                 |     }
                 |}
                 |""".stripMargin) shouldEqual Left(
        ConfeeCheckerErrors(
          List(
            ConfeeCheckerError(Location(18, 24), "Type error: 'value' value must have Number type")
          )
        )
      )
    }

  }

  def checkAST(input: String): Either[ConfeeError, ConfeeAST] =
    for {
      tokens      <- ConfeeLexer(input)
      parsed      <- ConfeeParser(tokens)
      validated   <- ConfeeValidator(parsed)
      bound       <- ConfeeBinder(validated)
      evaluated   <- ConfeeEvaluator(bound)
      constructed <- ConfeeConstructor(evaluated)
      checked     <- ConfeeChecker(constructed)
    } yield checked

}
