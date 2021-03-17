package com.spotify.confee

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfeeFormatterTest extends AnyFunSpec with Matchers {
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

    it("should format a config with proto") {
      formatJSON(
        """conf foo : Foo {
          |     a = {
          |          x = true
          |          y = 1.0
          |          z = "abc"
          |     }
          |     b = a {
          |          x = false
          |          y = 2.0
          |          z = "efg"
          |     }
          |}""".stripMargin,
        "foo",
        Right("""{
                |    "a" : {
                |        "x" : true,
                |        "y" : 1.0,
                |        "z" : "abc"
                |    },
                |    "b" : {
                |        "x" : false,
                |        "y" : 2.0,
                |        "z" : "efg"
                |    }
                |}""".stripMargin)
      )
    }

    it("should format a config with proto having reference and expression evaluation") {
      formatJSON(
        """conf foo : Foo {
          |     base = 1
          |     a = {
          |          priority = 1
          |          order = priority * base
          |     }
          |     b = a {
          |          priority = 2
          |          order = priority * base
          |     }
          |}""".stripMargin,
        "foo",
        Right("""{
                |    "base" : 1.0,
                |    "a" : {
                |        "priority" : 1.0,
                |        "order" : 1.0
                |    },
                |    "b" : {
                |        "priority" : 2.0,
                |        "order" : 2.0
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
      tokens      <- ConfeeLexer(input)
      parsed      <- ConfeeParser(tokens)
      linked      <- ConfeeLinker(parsed)
      validated   <- ConfeeValidator(linked)
      bound       <- ConfeeBinder(validated)
      evaluated   <- ConfeeEvaluator(bound)
      constructed <- ConfeeConstructor(evaluated)
      checked     <- ConfeeChecker(constructed)
      formatted   <- ConfeeFormatter(checked, conf, target)
    } yield formatted
  }
}
