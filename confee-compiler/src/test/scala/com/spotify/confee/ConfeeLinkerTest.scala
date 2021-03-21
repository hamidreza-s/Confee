package com.spotify.confee

import com.spotify.confee.ConfeeLinker.Code
import org.scalamock.scalatest.MockFactory
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeLinkerTest extends AnyFunSpec with Matchers with MockFactory {

  describe("Linker on imported files") {
    it("should link one confee file having types") {

      val mockReader = mock[ConfeeLinker.Reader]
      (mockReader.read _)
        .expects("data-type.confee")
        .returning(
          Some(
            Code(
              "data-type.confee",
              """
              |type Foo {
              |    a : Bool
              |    b : Number
              |}
              |
              |type Bar {
              |    a : String
              |    b : Foo
              |}
              |""".stripMargin
            )
          )
        )
        .once()

      linkAST("""
          |import "data-type.confee"
          |""".stripMargin, mockReader) shouldEqual Right(
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
            TypeStmt(
              NameToken("Bar"),
              TypeItems(
                List(
                  TypeItem(TypeItemKey("a"), TypeDef(NameToken("String"), isList = false)),
                  TypeItem(TypeItemKey("b"), TypeDef(NameToken("Foo"), isList = false))
                )
              )
            ),
            ImportStmt(StringToken("data-type.confee"))
          )
        )
      )
    }
  }

  def linkAST(input: String, reader: ConfeeLinker.Reader): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens <- ConfeeLexer(input)
      parsed <- ConfeeParser(tokens)
      linked <- ConfeeLinker(parsed, reader)
    } yield linked
  }
}
