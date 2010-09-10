package docsearch.query

object QMemType extends Enumeration {
  type QMemType = Value
  val Def = Value("def")
  val Val = Value("val")
  val Var = Value("var")
}

import QMemType._

case class Type(str: String)

case class Query( path: Option[QPath], 
                  memType: Option[QMemType],
                  name: Option[String],
                  args: List[List[QArg]],
                  resultType: Type)
                  
case class QPath(components: List[String])

case class QArg(name: Option[String], typ: Type)


