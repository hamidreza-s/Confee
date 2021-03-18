package com.spotify.confee

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeValidatorTest extends AnyFunSpec with Matchers {

  describe("Validator on type statements") {
    it("should NOT let types, confs, and their items with duplicated names (sorted errors)") {
      validateAST("""
          |type Foo { a : Bool }
          |type Foo { b : Bool }
          |
          |type Bar {
          |     c : Bool
          |     c : Bool
          |}
          |
          |conf foo : Foo {}
          |conf foo : Foo {}
          |
          |conf bar : Bar {
          |     c = true
          |     c = false
          |}
          |""".stripMargin) shouldEqual Left(
        ConfeeValidatorErrors(
          List(
            ConfeeValidatorError(
              Location(2, 6),
              "Validator error: duplicated type name: NameToken(Foo)"
            ),
            ConfeeValidatorError(
              Location(3, 6),
              "Validator error: duplicated type name: NameToken(Foo)"
            ),
            ConfeeValidatorError(
              Location(6, 10),
              "Validator error: duplicated item name of type: TypeItemKey(c)"
            ),
            ConfeeValidatorError(
              Location(7, 10),
              "Validator error: duplicated item name of type: TypeItemKey(c)"
            ),
            ConfeeValidatorError(
              Location(10, 6),
              "Validator error: duplicate conf name: WordToken(foo)"
            ),
            ConfeeValidatorError(
              Location(11, 6),
              "Validator error: duplicate conf name: WordToken(foo)"
            ),
            ConfeeValidatorError(
              Location(14, 10),
              "Validation error: duplicated item name of conf: ConfItemKey(c)"
            ),
            ConfeeValidatorError(
              Location(15, 10),
              "Validation error: duplicated item name of conf: ConfItemKey(c)"
            )
          )
        )
      )
    }
  }

  def validateAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens    <- ConfeeLexer(input)
      parsed    <- ConfeeParser(tokens)
      linked    <- ConfeeLinker(parsed)
      validated <- ConfeeValidator(linked)
    } yield validated
  }

}
