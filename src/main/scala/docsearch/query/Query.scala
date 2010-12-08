package docsearch.query

import docsearch.types.MemType._

import docsearch.types.Member

import net.liftweb.mapper._

case class QType(str: String, typeParams: List[QType]){
  override def toString = str + {if (!typeParams.isEmpty) typeParams.mkString("[", ",", "]") else ""}
}

case class Query( path: Option[QPath],
                  memType: Option[MemType],
                  name: Option[String],
                  args: List[List[QArg]],
                  resultType: QType) {
  override def toString = {
    "Query: path: " + path.toString +
    "\n\tmem type: " + memType.toString +
    "\n\tname: " + name.toString +
    "\n\targs: " + args.toString +
    "\n\tresult type: " + resultType
  }
  def findMatching(): List[Member] = {
    val queryParams =
      //FIXME this path is wrong
      /* List(Like(Member.entityToString, path.toString + "#%")) ++ */
      (memType map (By(Member.memType, _)) toList) ++
      (name map (By(Member.name, _)) toList) //++
      //(args) ++
      //(resultType)
    Member.findAll(queryParams:_*)
  }
}

case class QPath(components: List[String]){
  override def toString = components.mkString(".")
}

case class QArg(name: Option[String], typ: QType){
  override def toString = name match {
    case Some(n) => n + ": " + typ.toString
    case None => typ.toString
  }
}


