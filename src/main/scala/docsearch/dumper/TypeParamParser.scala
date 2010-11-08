package docsearch.dumper

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import docsearch.types._

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

class TPParser extends RegexParsers {  
  def id = "[a-zA-Z][a-zA-Z0-9_]*".r | "_"
  def param: Parser[TypeParam] = id~opt(('['~>rep1sep(param, ','))<~']') ^^ {
    case name ~ params => {
      val kind: Kind = Kind.makeKind(params getOrElse List())
      println("Kind:" + kind)
      val tp: TypeParam = TypeParam.create.name(name).kind(kind)
      tp.save
      tp
    }
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
      parse(input, param)
      test()
    }
  }
  if (!DB.jndiJdbcConnAvailable_?) {
    val vendor =
      new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
                           Props.get("db.url") openOr
                           "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
                           Props.get("db.user"), Props.get("db.password"))

    DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
  }
  test()
}
