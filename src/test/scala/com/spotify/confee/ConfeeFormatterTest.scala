package com.spotify.confee

import org.scalatest.{FunSpec, Matchers}

class ConfeeFormatterTest extends FunSpec with Matchers {
  describe("JSON Formatter") {
    it("should format a config with bool, number, string, array and object") {
      formatJSON(
        """conf foo : Foo {
          |     a = true
          |     b = 1.0
          |     c = "abc"
          |     d = ["a", "b", "c"]
          |     e = {
          |          a = true
          |          b = 1.0
          |          c = "abc"
          |          d = ["a", "b", "c"]
          |          e = {
          |               a = true
          |               b = 1.0
          |               c = "abc"
          |               d = ["a", "b", "c"]
          |          }
          |     }
          |}""".stripMargin,
        "foo",
        Right("""{
          |    "a" : true,
          |    "b" : 1.0,
          |    "c" : "abc",
          |    "d" : [
          |        "a",
          |        "b",
          |        "c"
          |    ],
          |    "e" : {
          |        "a" : true,
          |        "b" : 1.0,
          |        "c" : "abc",
          |        "d" : [
          |            "a",
          |            "b",
          |            "c"
          |        ],
          |        "e" : {
          |            "a" : true,
          |            "b" : 1.0,
          |            "c" : "abc",
          |            "d" : [
          |                "a",
          |                "b",
          |                "c"
          |            ]
          |        }
          |    }
          |}""".stripMargin)
      )
    }
  }

  def formatJSON(input: String, conf: String, output: Either[ConfeeError, String]): Unit =
    formatAST(input, conf, ConfeeCompiler.JSON) shouldEqual output

  def formatAST(
      input: String,
      conf: String,
      target: ConfeeCompiler.Target
  ): Either[ConfeeError, String] = {
    for {
      tokens    <- ConfeeLexer(input).right
      parsed    <- ConfeeParser(tokens).right
      bound     <- ConfeeBinder(parsed).right
      evaluated <- ConfeeEvaluator(bound).right
      checked   <- ConfeeChecker(evaluated).right
      formatted <- ConfeeFormatter(checked, conf, target)
    } yield formatted
  }
}
