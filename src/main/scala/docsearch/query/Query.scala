package docsearch.query

import docsearch.types.MemType._

import docsearch.types.Member

import net.liftweb.mapper._

sealed abstract class QType
case class QTuple(elems: List[QType]) extends QType
case class QFunc(args: List[QType]) extends QType
case class QTVar(name: String) extends QType
case class QTName(name: String) extends QType
case class QTApp(op: QType, args: List[QType]) extends QType

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


