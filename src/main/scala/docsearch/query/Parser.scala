package docsearch.query

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.lexical.{Lexical}
import scala.util.parsing.combinator.syntactical.{TokenParsers}
import scala.util.parsing.combinator.testing.Tester
import docsearch.types.MemType._


trait QueryTokens extends Tokens {
  case class Delim(chars: String) extends Token {
    def pprint = chars
  }
  
  case class Id(chars: String) extends Token {
    def pprint = "ID: " + chars
  }
  
  case class Reserved(chars: String) extends Token {
    def pprint = "Reserved: " + chars
  }
  
}

class QueryLexer extends Lexical with QueryTokens with RegexParsers {
  type Tokens <: QueryTokens
  override type Elem = Char

  // Treat brackets as whitespace, as they only show up in existentials and structurals,
  // which are not supported
  def whitespace: Parser[Any] = 
    rep(whitespaceChar)

  //Begin Scala identifiers, as defined on page 4 of the spec.
  //Start with character types.
  val notOpChars = List('(', ')', '[', ']', '{', '}', '\'', '`', '"', '.', ';', ',', '$', '_')
  def opchar: Parser[Elem] = elem("opchar", {ch => 
    ('\u0020' to '\u007F' contains ch) &&
    !(notOpChars contains ch) &&
    !('a' to 'z' contains ch) &&
    !('A' to 'Z' contains ch) &&
    !('0' to '9' contains ch) &&
    ch != ' '
  })
  def lower: Parser[Elem] = elem("lowercase letter", {ch => ch >= 'a' && ch <= 'z'})
  def upper: Parser[Elem] = 
    elem("uppercase letter, _, or $", {ch => (ch >= 'A' && ch <= 'Z') || ch == '_' || ch == '$'})
  override def letter: Parser[Elem] = lower | upper
  override def digit: Parser[Elem] = elem("digit", {ch => ch >= '0' && ch <= '9'})
  
  //Now, take the grammar from page 4 of the spec
  def op: Parser[List[Elem]] = rep1(opchar)

  def varid: Parser[List[Elem]] = lower~idrest ^^ {
    case c ~ cs => c :: cs
  }

  def plainid: Parser[List[Elem]] = 
    ( upper~idrest ^^ {case ch ~ chs => ch :: chs}
    ||| varid
    ||| op
    )

  def letterNot_ : Parser[Elem] = 
    lower | elem("letter", (ch => ('A' to 'Z' contains ch) || ch == '$'))
  
  def idrest: Parser[List[Elem]] = """[a-zA-Z0-9_\$]*_""".r ~ op ^^ {
    case str ~ op => (str toList)++op
  } |||
  '_'~>op ^^ ('_'::_) |||
  rep(letter | digit)
  

  def id: Parser[List[Elem]] = 
    ( plainid 
    ||| ('`'~> rep1(elem("not backtick", (_!='`')))) <~ '`'
    )
  

  //End names

  def reservedWord: Parser[List[Elem]] = {
    val words = for {word <- reservedWords} yield (accept(word.toList))
    words.tail.foldRight(words.head)(_|_)
  }

  private val reservedWords:List[String] = List(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends", 
    "false", "final", "finally", "for", "forSome", "if", "implicit",
    "import", "lazy", "match", "new", "null", "object", "override", 
    "package", "private", "protected", "requires", "return", "sealed",
    "super", "this", "throw", "trait", "try", "true", "type", "val",  
    "var", "while", "with", "yield", "_", ":", "=", "=>", "<-", "<:",
    "<%", ">:", "#", "@")

  def token: Parser[Token] = (
      id ^^ {chars: List[Char] => {
        val name = chars.mkString
        if (reservedWords contains name) Reserved(name) else Id(name)
      }}
    | accept('.') ^^^ Delim(".")
    | accept('(') ^^^ Delim("(")
    | accept(')') ^^^ Delim(")")
    | accept(',') ^^^ Delim(",")
    | accept('[') ^^^ Delim("[")
    | accept(']') ^^^ Delim("]")
    | EofCh ^^^ EOF
  )
}

class QueryParser(val lexical: QueryLexer = new QueryLexer) extends TokenParsers with PackratParsers with QueryTokens {
  type Tokens = QueryTokens
  
  implicit def string2Delim(str: String): Parser[Any] = 
    elem("delimiter "+str, {
      case lexical.Delim(x) => x == str
      case _ => false
    })
  lazy val pathPart: PackratParser[String] = elem("id", _.isInstanceOf[lexical.Id] ) ^^ {
    case lexical.Id(x) => x
  }
  
  lazy val path: PackratParser[QPath] = rep1sep(pathPart, ".") ^^ {
    elts: List[String] => QPath(elts)
  }
  
  lazy val memType: PackratParser[MemType] = elem("member type", {
    case lexical.Reserved(str) if (List("var", "val", "def") contains str) => true
    case _ => false 
  }) ^^ {
    case lexical.Reserved("var") => Var
    case lexical.Reserved("val") => Val
    case lexical.Reserved("def") => Def
  }
  
  lazy val name: PackratParser[String] = elem("name", _.isInstanceOf[lexical.Id] ) ^^ {
    case lexical.Id(x) => x
  }
  
  lazy val memTypeNameArgs: PackratParser[(Option[MemType], Option[String], List[List[QArg]])] = 
    opt(memType)~opt(name)~rep(argList) ^^ {
      case mt~n~al => (mt, n, al)
    }

  lazy val typ: PackratParser[QType] = elem("type", _.isInstanceOf[lexical.Id] )~opt(typeParam) ^^ {
    case lexical.Id(x)~p => QType(x, p.toList.flatten)
  }
  
  lazy val typeParam: PackratParser[List[QType]] = "["~>repsep(typ, ",")<~"]"
  
  lazy val hash: PackratParser[Any] = elem("'#'", {
    case lexical.Reserved("#") => true
    case _ => false
  })
  
  lazy val colon: PackratParser[Any] = elem("':'", {
    case lexical.Reserved(":") => true
    case _ => false
  }) 
  
  lazy val arg: PackratParser[QArg] = opt(name<~colon)~typ ^^ {
    case n~t => QArg(n,t)
  }
  
  lazy val argList: PackratParser[List[QArg]] = "("~>(repsep(arg,",")<~")")
  
  lazy val query: PackratParser[Query] = opt(path<~hash)~opt(memTypeNameArgs<~colon)~typ ^^ {
    case p~Some((mt, n, a))~t => Query(p, mt, n, a, t)
    case p~None~t => Query(p, None, None, List(), t)
  }

  /**
   * A simple way to run the parser.
   */
  def parse(input: String, toParse:Parser[Any]) = {
    phrase(toParse)(new lexical.Scanner(input))
  } 
}


object TestQueryLexer extends QueryLexer with Application {
  def test(): Unit = {
    print("----LEXER> ")
    val input = Console.readLine()
    if (input != "q") {
      var scan = new Scanner(input)
      var result = collection.mutable.ListBuffer[Token]()
      while(!scan.atEnd) {
        result += scan.first
        scan = scan.rest
      }
      println(result toList)
      test()
    }
  }
  
  test()
}

object TestQueryParser extends QueryParser with Application {
  def test():Unit = {
    print("------PARSER> ")
    val input = Console.readLine()
    if (input != "q") {
      var scan = new lexical.Scanner(input)
      var lexed = collection.mutable.ListBuffer[lexical.Token]()
      while (!scan.atEnd) {
        lexed += scan.first
        scan = scan.rest
      }
      print("Tokens: ")
      println(lexed toList)
      print("Query: ")
      println(parse(input, query))
      test()
    } 
  }

  test()
}

