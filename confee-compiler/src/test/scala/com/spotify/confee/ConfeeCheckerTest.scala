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
