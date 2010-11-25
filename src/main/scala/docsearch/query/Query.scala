package docsearch.query

import docsearch.types.MemType._

import docsearch.types.Member

import net.liftweb.mapper._

case class Type(str: String)

case class Query( path: Option[QPath], 
                  memType: Option[MemType],
                  name: Option[String],
                  args: List[List[QArg]],
                  resultType: Type) {
  def findMatching(): List[Member] = {
    val queryParams = 
      (memType map (By(Member.memType, _)) toList) ++
      (name map (By(Member.name, _)) toList) /* FIXME Add the rest! */
    Member.findAll(queryParams:_*)
  }
}
                  
case class QPath(components: List[String])

case class QArg(name: Option[String], typ: Type)


