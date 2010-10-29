package docsearch.dumper

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import docsearch.types._

case class TypeParam(name: String, params: List[TypeParam]) {
  private[this] def paramKind(params: List[TypeParam]): Kind = params match {
    case Nil => *
    case p :: ps => p.kind --> paramKind(ps)
  } 

  def kind(): Kind = paramKind(params)

  override def toString = name +
    (params match {
      case Nil => ""
      case ps => "[" + ps.mkString(",") + "]"
    }) + 
    " : " + this.kind
}

class TPParser extends RegexParsers {
  def id = "[a-zA-Z][a-zA-Z0-9_]*".r | "_"
  def param: Parser[TypeParam] = id~opt(('['~>rep1sep(param, ','))<~']') ^^ {
    case name ~ params => TypeParam(name, params getOrElse List())
  }

  /**
   * A simple way to run the parser.
   */
  def parse(input: String, toParse: Parser[Any]) = {
    phrase(toParse)(new CharSequenceReader(input))
  }

  def parseParam(input: String) = parse(input, param)
}

object ParamParserTest extends TPParser with Application {
  def test(): Unit = {
    print("------PARAM PARSER> ")
    val input = Console.readLine()
    if (input != "q") {
      println(parse(input, param))
      test()
    }
  }
  test()
}
