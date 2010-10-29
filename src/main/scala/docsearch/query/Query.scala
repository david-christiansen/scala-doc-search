package docsearch.query

import docsearch.types.MemType._

case class Type(str: String)

case class Query( path: Option[QPath], 
                  memType: Option[MemType],
                  name: Option[String],
                  args: List[List[QArg]],
                  resultType: Type)
                  
case class QPath(components: List[String])

case class QArg(name: Option[String], typ: Type)


