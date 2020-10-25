package com.spotify.confee

import com.spotify.confee.ConfeeBinder.IndexRow
import org.scalatest.{FunSpec, Matchers}

class ConfeeBinderTest extends FunSpec with Matchers {

  describe("Indexer on config items") {
    it("should index config items and its object or proto children WITHOUT reference") {
      indexAST("""conf foo : Foo {
                 |     a = true
                 |     b = "abc"
                 |     c = 1
                 |     d = [1]
                 |     e = {bar = 1 bat = {ban = 2}}
                 |     f = g {bar = 1 bat = {ban = 2}}
                 |}""".stripMargin) shouldEqual List(
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
          hasReference = false
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("f"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
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
          hasReference = false
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("f"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(2)),
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
          hasReference = false
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("e"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
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
          hasReference = false
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("e"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(2)),
          hasReference = false
        ),
        IndexRow(
          WordToken("d"),
          List(WordToken("foo")),
          LiteralArray(List(LiteralNumberFactor(NumberToken(1.0)))),
          hasReference = false
        ),
        IndexRow(
          WordToken("c"),
          List(WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          hasReference = false
        ),
        IndexRow(
          WordToken("b"),
          List(WordToken("foo")),
          LiteralStringFactor(StringToken("abc")),
          hasReference = false
        ),
        IndexRow(
          WordToken("a"),
          List(WordToken("foo")),
          LiteralBoolFactor(BoolToken(true)),
          hasReference = false
        )
      )
    }

    it("should index config items and its object or proto children WITH reference") {
      indexAST("""conf foo : Foo {
                 |     a = false and ref1
                 |     b = "abc" + ref2
                 |     c = 1 + ref3
                 |     d = [ref4]
                 |     e = {bar = 1 bat = {ban = ref5}}
                 |     f = g {bar = 1 bat = {ban = ref6}}
                 |}""".stripMargin) shouldEqual List(
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
                        LiteralObjectItem(WordToken("ban"), LiteralNumberWord(WordToken("ref6")))
                      )
                    )
                  )
                )
              )
            )
          ),
          hasReference = true
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("f"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          hasReference = false
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("f"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralNumberWord(WordToken("ref6"))))
            )
          ),
          hasReference = true
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("f"), WordToken("foo")),
          LiteralNumberWord(WordToken("ref6")),
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
                        LiteralObjectItem(WordToken("ban"), LiteralNumberWord(WordToken("ref5")))
                      )
                    )
                  )
                )
              )
            )
          ),
          hasReference = true
        ),
        IndexRow(
          WordToken("bar"),
          List(WordToken("e"), WordToken("foo")),
          LiteralNumberFactor(NumberToken(1)),
          hasReference = false
        ),
        IndexRow(
          WordToken("bat"),
          List(WordToken("e"), WordToken("foo")),
          LiteralObject(
            LiteralObjectItems(
              List(LiteralObjectItem(WordToken("ban"), LiteralNumberWord(WordToken("ref5"))))
            )
          ),
          hasReference = true
        ),
        IndexRow(
          WordToken("ban"),
          List(WordToken("bat"), WordToken("e"), WordToken("foo")),
          LiteralNumberWord(WordToken("ref5")),
          hasReference = true
        ),
        IndexRow(
          WordToken("d"),
          List(WordToken("foo")),
          LiteralArray(List(LiteralNumberWord(WordToken("ref4")))),
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
          hasReference = true
        ),
        IndexRow(
          WordToken("a"),
          List(WordToken("foo")),
          LiteralBoolGroup(
            LiteralBoolOperatorAnd(),
            LiteralBoolFactor(BoolToken(false)),
            LiteralBoolWord(WordToken("ref1"))
          ),
          hasReference = true
        )
      )
    }
  }

  def indexAST(input: String): List[IndexRow] = {
    (for {
      tokens <- ConfeeLexer(input).right
      parsed <- ConfeeParser(tokens).right
    } yield parsed) match {
      case Right(Grammar(stmts: List[Stmt])) => ConfeeBinder.indexStmts(stmts)
      case error                             => fail(error.toString)
    }
  }

  def bindAST(input: String): Either[ConfeeError, ConfeeAST] = {
    for {
      tokens <- ConfeeLexer(input).right
      parsed <- ConfeeParser(tokens).right
      bound  <- ConfeeBinder(parsed)
    } yield bound
  }
}
