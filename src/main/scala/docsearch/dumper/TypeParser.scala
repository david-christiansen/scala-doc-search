package docsearch.dumper

import docsearch.types._


import scala.util.parsing.combinator._
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.lexical.{Lexical}
import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.combinator.syntactical.{TokenParsers}

/**
 * Define the token types to be returned by the lexer.
 */
trait TypeTokens extends Tokens {
  /**
   * A token representing a Scala type name
   */
  case class Name(chars: String) extends Token {
    override def toString = "`" + chars + "`"
  }

  /**
   * A token representing a type variable
   */
  case class VarName(chars: String) extends Token {
    override def toString = "'"+chars+"'"
  }

  /**
   * A token representing some kind of symbol, like ( or =>
   */
  case class Delim(chars: String) extends Token {
    override def toString = chars
  }

  /**
   * A token representing an annotation
   */ 
  case class Annotation(chars: String) extends Token {
    override def toString = "@"+chars
  }
  
  /**
   * A token representing the introduction of an existential type
   */
  case class Existential(chars: String) extends Token {
    override def toString = "forSome"
  }

  /**
   * A token representing a Scala wildcard
   */
  case class Wildcard(chars: String) extends Token {
    override def toString = "_"
  }
}

/**
 * A lexer for Scala types. It transforms a String into a stream of TypeTokens,
 * making whitespace and such irrelevant.
 */
class TypeLexer extends Lexical with TypeTokens with RegexParsers {
  type Tokens <: TypeTokens
  override type Elem = Char
  
  import scala.util.parsing.input.CharArrayReader.EofCh

  // Treat brackets as whitespace, as they only show up in existentials and structurals,
  // which are not supported
  def whitespace: Parser[Any] = 
    rep(whitespaceChar | '{'~inBraces)
  def inBraces: Parser[Any] = '{'~inBraces | chrExcept(EofCh) ~ inBraces | '}'


  implicit def elem2Parser(e: Elem):Parser[Elem] = elem(e.toString, (_==e))

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
  
  def idWithPath: Parser[String] = rep1sep(id, '.') ^^ {components =>
    components.map(_.mkString).mkString(".")
  }
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

  def annotation: Parser[Token] = '@'~>idWithPath ^^ {str => Annotation(str)}
  def existential: Parser[Token] = accept("forSome".toList) ^^^ Existential("forSome")

  def nameOrVar(name: String) = 
    if (name.length == 1) VarName(name) else Name(name)

  def token: Parser[Token] = (
      accept("with".toList) ^^^ Delim("with")
    | annotation
    | existential
    | idWithPath ^^ {path =>
        val notInPath = reservedWords filterNot (List("this", "type") contains)
        val components = path.split('.')
        if (path == "_") Wildcard("_")
        else if (List("=>", "<:", ">:") contains path) Delim(path)
        else if (components.toList.forall(x => !notInPath.contains(x)))
          nameOrVar(components(components.length - 1)) // Throw out path!
        else
          ErrorToken("Cannot use reserved word as identifier in "+path)
      }
    | accept('(') ^^^ Delim("(")
    | accept(')') ^^^ Delim(")")
    | accept(',') ^^^ Delim(",")
    | accept('[') ^^^ Delim("[")
    | accept(']') ^^^ Delim("]")
    | EofCh ^^^ EOF
  )
}

class ScalaDocTypeLexer(typeVars: List[String]) extends TypeLexer {
  override def nameOrVar(name: String) =
    if (typeVars contains name) VarName(name) else Name(name)
}

object SymbolGenerator {
  var count = -1

  def gensym() = {
    count = count + 1
    "$" + ((count + 'A'.toInt).toChar) + "$"
  }

  def nGensyms(n: Int): List[String] = if (n > 0) gensym() :: nGensyms(n - 1) else Nil
}

/**
 * A parser that transforms a stream of TypeTokens into a ScalaType instance. 
 */
class TypeParser(val lexical: TypeLexer = new TypeLexer) extends TokenParsers with PackratParsers with TypeTokens {
  type Tokens = TypeTokens
//  val lexical: TypeLexer// = new TypeLexer

  /**
   * Allow the use of delimiter strings directly in the grammer, so one can type
   *  "(" ~ rep(name, ",") ~ ")"
   * instead of
   *  delim("(") ~ rep(name, delim(",") ~ delim(")")
   *
   * @param str the delimiter to convert
   * @return a parser for the delimiter
   */
  implicit def string2Delim(str: String): Parser[Any] = 
    elem("delimiter "+str, {
      case lexical.Delim(x) => x == str
      case _ => false
    })

  lazy val scalaType: PackratParser[Type] = 
    ( wildcard
    | withTraits<~(rep(annotation|existential))
    | generic<~opt(star)<~(rep(annotation|existential))
    | function
    | tuple
    | typeVar<~opt(star)<~(opt(bounds)~rep(annotation|existential))
    | typeName<~opt(star) <~(rep(annotation|existential))
    | ("("~>scalaType)<~")"
    )
  
  lazy val typeName: PackratParser[Type] =
    elem("type name", _.isInstanceOf[lexical.Name]) ^^ {
      case lexical.Name(chars) => Type.createConcreteType(chars, List())
    }

  lazy val typeVar: PackratParser[Type] =
    elem("type variable", _.isInstanceOf[lexical.VarName]) ^^ {
      case lexical.VarName(chars) => Type.createTypeVar(chars, List())
    }

  lazy val generic: PackratParser[Type] =
    (typeName|typeVar)~(("["~>rep1sep(scalaType<~opt(bounds), ","))<~"]")<~rep("["~rep1sep(scalaType, ",")~"]") ^^ {
      //FIXME should not use name to string but something better
      case name ~ args => if (name.typeType == TypeType.ConcreteType) Type.addConcreteTypeParams(name, args)
                          else Type.addTypeVarParams(name, args)
      case _ => error("Fawiled to parse generic")
    }

 
  lazy val wildcard: PackratParser[Type] =
    elem("underscore", _.isInstanceOf[lexical.Wildcard]) ^^ {
      _ => Type
    }
  //FIXME This is wrong
  lazy val function: PackratParser[Type] = 
    rep1(("("~> repsep(funcParam, ","))<~ ")")~opt("=>")~scalaType ^^ {
      case args ~ _ ~ ret => Type.createFunction(args, ret)
    } |
    (scalaType<~"=>")~scalaType ^^ {
      case t1 ~ t2 => Type.createMethod(List(List(t1)), t2)
    }


  lazy val tuple: PackratParser[Type] =
    ("("~>((scalaType<~",")~rep1sep(scalaType, ",")))<~")" ^^ {
      case t ~ ts => Type.createTuple(t::ts)
    }
    
  lazy val withTraits: PackratParser[Type] = 
    ((generic | typeName)<~"with")~rep1sep((generic | typeName), "with") ^^ {
      //FIXME maybe add a add concrete type traits
      case t ~ ts => Type.addTraits(t, ts)
    }
    
  // Throw out type bounds
  lazy val bounds: PackratParser[Any] = rep(bound)
  lazy val bound: PackratParser[Any] = (">:" | "<:")~(generic | typeName | typeVar)

  lazy val funcParam: PackratParser[Type] = (opt("=>")~>(scalaType))

  //Ugly hack to recognize * after type
  lazy val star: PackratParser[Any] = elem("star", _ match {
    case lexical.Name("*") => true
    case lexical.VarName("*") => true
    case _ => false
  })
  lazy val byNameParam: PackratParser[Type] = "=>"~>(scalaType)  
  lazy val repeatedParam: PackratParser[Type] = scalaType<~"*"

  lazy val annotation: PackratParser[Any] =
    elem("annotation", _.isInstanceOf[Annotation])<~opt("["~rep1sep(scalaType, ",")~"]")

  lazy val existential: PackratParser[Any] =
    elem("forSome", _.isInstanceOf[Existential])

  /**
   * A simple way to run the parser.
   */
  def parse(input: String, toParse:Parser[Type] = scalaType) = {
    var scan = new lexical.Scanner(input)
    while (!scan.atEnd) {
      val tok = scan.first
      scan = scan.rest
    }
    phrase(toParse)(new lexical.Scanner(input))
  }  
}

object TestTypeLexer extends TypeLexer with Application {
  def test(): Unit = {
    print("> ")
    val input = Console.readLine()
    if (input != "q") {
      println(phrase(token)(new scala.util.parsing.input.CharSequenceReader(input)))
      test()
    }     
  }
  test()
}

object TestTypeParser extends TypeParser with Application {
  import bootstrap.liftweb.Boot
  val boot = (new Boot)
  boot.boot
  def test():Unit = {
    print("> ")
    val input = Console.readLine()
    if (input != "q") {
      println(parse(input, scalaType))
      test()
    } 
  }

  test()
}
