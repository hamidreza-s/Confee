package example

import scala.util.parsing.combinator._

trait Tree
case class Node(op: Char, left: Tree, right: Tree) extends Tree
case class Leaf(value: BigInt) extends Tree

class Calc extends RegexParsers {

  def number: Parser[Tree] = """-?\d+""".r ^^ { s => Leaf(BigInt(s)) }

  def operator: Parser[Char] = ("+" | "-" | "*" | "/") ^^ {
    case "+" => '+'
    case "-" => '-'
    case "*" => '*'
    case "/" => '/'
  }

  def expr: Parser[Tree] = factor ||| factor ~ operator ~ expr ^^ {
    case x ~ op ~ y => Node(op, x, y)
  }

  def factor: Parser[Tree] = number | "(" ~> expr <~ ")"
}

object Calculator extends Calc {
  def main(args: Array[String]) {
    val input = "0 + 1 + 2 + (3 / 4 * (5 - 6))"
    val output = parseAll(expr, input)
    println(input + " = " + output)
  }
}
