package docsearch.query

import docsearch.types.MemType._

import docsearch.types.Member

import net.liftweb.mapper._

case class Type(str: String, typeParams: List[Type]){
  override def toString = str + {if (!typeParams.isEmpty) typeParams.mkString("[", ",", "]") else ""}
}

case class Query( path: Option[QPath], 
                  memType: Option[MemType],
                  name: Option[String],
                  args: List[List[QArg]],
                  resultType: Type) {
  override def toString = {
    "\n\tpath: " + path.toString +
    "\n\tmem type: " + memType.toString +
    "\n\tname: " + name.toString +
    "\n\targs: " + args.toString + 
    "\n\tresult type: " + resultType
  }
}
                  
case class QPath(components: List[String])

case class QArg(name: Option[String], typ: Type)


